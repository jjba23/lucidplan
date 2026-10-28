/* lucidplan - free and open project management for everyone
 *
 * SPDX-License-Identifier: MIT
 * Copyright © Josep Bigorra <jjbigorra@gmail.com>
*/

let draggedTicket = null;

// A ticket is dragged by its grip, never by its whole card or row: a drag
// source cannot be read with the mouse, because dragging across its text
// moves the ticket instead of selecting what it says. The grip is the only
// element that carries `draggable', so the row it points at is what travels
// -- both the backlog row and the kanban card mark themselves with
// `data-ticket-id', and either one is resolved from the grip that was grabbed.
function dragTicketStart(event) {
  const row = event.currentTarget.closest('[data-ticket-id]');
  if (!row) return;

  draggedTicket = row;
  event.dataTransfer.effectAllowed = 'move';
  // The kanban board matches the dropped id against the ids on this page, so
  // the id that travels is the ticket's human id rather than a DOM id.
  event.dataTransfer.setData('text/plain', row.getAttribute('data-ticket-id'));
  // What follows the pointer is the row, not the little grip it was
  // dragged by.
  event.dataTransfer.setDragImage(row, 24, 16);

  // The dragged row is faded, not made untouchable.  Chromium ends a drag
  // session the moment the node under the pointer stops being hit-testable,
  // so disabling the dragged element's pointer events here would make the
  // grip refuse to drag at all.  Every drop target resolves what is under the
  // cursor with `closest', so a hit on the card or an item inside it still
  // reaches the list or the zone that owns it.
  draggedTicket.classList.add('opacity-50');
}

function dragTicketEnd(event) {
  if (draggedTicket) {
    draggedTicket.classList.remove('opacity-50');
    draggedTicket = null;
  }
}

// Where a drag is carried over is answered twice: once when the pointer enters
// the list or the column, and again on every move across it.  Chromium treats
// the element whose last `dragenter' or `dragover' was answered as the drop
// target and reads the release against that one element, so a list that only
// answers `dragover' can be left with no target at all when the pointer
// reaches it over something else -- the release is then not a drop at all, and
// the ticket stays where it was.  Both events are answered here, by the drop
// target itself.
function acceptDrag(event) {
    event.preventDefault();
    if (event.dataTransfer) {
        event.dataTransfer.dropEffect = 'move';
    }
}

function allowDrop(event) {
    event.preventDefault();
    event.dataTransfer.dropEffect = 'move';
    const target = event.target.closest('[data-ticket-id]');
    if (target && target !== draggedTicket) {
        const rect = target.getBoundingClientRect();
        const isAfter = (event.clientY - rect.top) / (rect.bottom - rect.top) > 0.5;

        // Clear previous drop indicators across all items
        document.querySelectorAll('.drop-target-above, .drop-target-below')
            .forEach(el => el.classList.remove('drop-target-above', 'drop-target-below', 'border-t-4', 'border-b-4', 'border-jeans-blue-500'));

        // Highlight placement edge
        if (isAfter) {
            target.classList.add('drop-target-below', 'border-b-4', 'border-jeans-blue-500');
        } else {
            target.classList.add('drop-target-above', 'border-t-4', 'border-jeans-blue-500');
        }
    }
}

async function dropTicketsItem(event) {
  event.preventDefault();
  if (!draggedTicket) return;

  const list = document.getElementById('lucidplan-tickets-list');
  const projectId = list.getAttribute('data-project-id');

  // Find the target ticket element under the drop cursor
  const dropTarget = event.target.closest('[data-ticket-id]');

  if (dropTarget && dropTarget !== draggedTicket) {
    const rect = dropTarget.getBoundingClientRect();
    const isAfter = (event.clientY - rect.top) / (rect.bottom - rect.top) > 0.5;
    list.insertBefore(draggedTicket, isAfter ? dropTarget.nextSibling : dropTarget);
  } else if (!dropTarget && event.target === list) {
    list.appendChild(draggedTicket);
  }

  // Restore the row's ordinary look
  draggedTicket.classList.remove('opacity-50');

  // Query direct list item children in their NEW DOM order
  const ticketNodes = Array.from(list.querySelectorAll(':scope > [data-ticket-id]'));

  // The list is shown by priority, and it says which way round: under the
  // ascending order the topmost row carries the smallest number, under the
  // descending one the largest.  The number written is the one that puts the
  // row back in the place it was just put, under the order being shown.
  const ascending = list.getAttribute('data-sort') === 'priority-asc';

  const reorderedTickets = ticketNodes.map((node, index) => ({
    id: node.getAttribute('data-ticket-id'),
    priority: ascending ? index + 1 : ticketNodes.length - index
  }));

  try {
    const response = await fetch(`/tickets/${projectId}/reorder`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ tickets: reorderedTickets })
    });

    if (!response.ok) {
      console.error('Failed to save priority order');
    }
  } catch (err) {
    console.error('Network error updating priorities:', err);
  }
}

// ==================================
// A kanban card is dragged by the same grip the backlog row uses, so its
// start is `dragTicketStart' above rather than a second function saying the
// same thing.

function handleKanbanDragOver(e) {
  e.preventDefault();
  e.dataTransfer.dropEffect = "move";

  // Highlight drop zone
  const dropZone = e.currentTarget.closest(".ticket-drop-zone");
  if (dropZone) {
    dropZone.classList.add("bg-stone-300/75", "dark:bg-onyx-700/75");
  }
}

function handleKanbanDragLeave(e) {
  const dropZone = e.currentTarget.closest(".ticket-drop-zone");
  if (dropZone) {
    dropZone.classList.remove("bg-stone-300/75", "dark:bg-onyx-700/75");
  }
}

async function handleKanbanDrop(e, project) {
  e.preventDefault();

  const dropZone = e.currentTarget.closest(".ticket-drop-zone");
  if (!dropZone) return;

  dropZone.classList.remove("bg-stone-300/75", "dark:bg-onyx-700/75");

  const ticketId = e.dataTransfer.getData("text/plain");
  const newStatus = dropZone.dataset.status;
  const ticketElement = document.querySelector(`[data-ticket-id="${ticketId}"]`);

  if (!ticketElement || !newStatus) return;

  // Optimistically move ticket DOM element to target drop zone
  dropZone.appendChild(ticketElement);

  // Send API request to update status in Artanis backend
  try {
    const response = await fetch(`/tickets/${project}/id/${ticketId}/status`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ status: newStatus })
    });

    if (!response.ok) {
      window.location.reload(); // Revert layout on server failure
    }
  } catch (err) {
    console.error("Failed to update ticket status:", err);
    window.location.reload();
  }
}

// ==================================
// Changing a ticket in place, from the ticket's own page
// ==================================

// A change is confirmed by the same frosted card the framework shows every
// notice in. The card sits in the page as an inert template and its wording
// travels with it as data, so no sentence is written into script, where it
// could not be translated. The tray is the polite live region the card was
// built for, so the confirmation is announced without interrupting a reader.
function showQuietNotice(title, body) {
  const tray = document.getElementById('notification-tray');
  const template = document.getElementById('lucidplan-quiet-notice');
  if (!tray || !template) return;

  // The tray hangs at the top of the viewport, which is where this page's own
  // sticky header is: the notice is dropped below the header, so it is read
  // instead of sitting half-behind the bar it was announced under.
  const header = document.querySelector('header');
  if (header) {
    tray.style.top = `${header.getBoundingClientRect().bottom + 12}px`;
  }

  tray.innerHTML = '';
  tray.appendChild(template.content.cloneNode(true));

  const titleNode = document.getElementById('notification-title');
  const bodyNode = document.getElementById('notification-body');
  if (titleNode) titleNode.textContent = title ?? '';
  if (bodyNode) bodyNode.textContent = body ?? '';

  setTimeout(() => { tray.innerHTML = ''; }, 3000);
}

// A row of a native menu is written as "<glyph> <label>", both as the row's
// own text, so the label a reader chose is what follows the first word.
function chosenOptionLabel(select) {
  const option = select.options[select.selectedIndex];
  const text = option ? option.textContent.trim() : '';
  const space = text.indexOf(' ');
  return space === -1 ? text : text.slice(space + 1);
}

// The small control on a ticket's own page posts its change as soon as it is
// changed and says so quietly: the notice names the field and the value it
// now holds, and a change that did not reach the server puts the menu back
// where it was rather than leaving the page claiming something untrue.
async function updateTicketField(select, project, humanId, field) {
  const wrapper = select.closest('[data-inline-field]');
  const template = document.getElementById('lucidplan-quiet-notice');
  const previous = wrapper ? wrapper.dataset.currentValue : undefined;

  try {
    const response = await fetch(`/tickets/${project}/id/${humanId}/field`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ field: field, value: select.value })
    });

    // A session that has aged out is not a saved change. The server answers
    // with the ticket page's own rule -- the login page -- so the reader goes
    // there instead of being told a change was written that was not.
    if (response.redirected) {
      window.location.assign(response.url);
      return;
    }

    if (!response.ok) throw new Error(`HTTP ${response.status}`);

    if (wrapper) wrapper.dataset.currentValue = select.value;
    showQuietNotice(template ? template.dataset.savedLabel : '',
                    `${wrapper ? wrapper.dataset.fieldLabel : field}: ${chosenOptionLabel(select)}`);
  } catch (err) {
    console.error('Failed to change the ticket:', err);
    if (previous !== undefined) select.value = previous;
    showQuietNotice(template ? template.dataset.failedLabel : '',
                    template ? template.dataset.failedBody : '');
  }
}
