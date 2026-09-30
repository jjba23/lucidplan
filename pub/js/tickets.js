let draggedTicket = null;

function dragTicketStart(event) {
  draggedTicket = event.currentTarget;
  event.dataTransfer.effectAllowed = 'move';
  event.dataTransfer.setData('text/plain', draggedTicket.id);

  // Disable pointer events on dragged element so mouse events hit the drop targets underneath
  draggedTicket.classList.add('opacity-50', 'pointer-events-none');
}

function dragTicketEnd(event) {
  if (draggedTicket) {
    draggedTicket.classList.remove('opacity-50', 'pointer-events-none');
    draggedTicket = null;
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

  // Restore pointer events
  draggedTicket.classList.remove('opacity-50', 'pointer-events-none');

  // Query direct list item children in their NEW DOM order
  const ticketNodes = Array.from(list.querySelectorAll(':scope > [data-ticket-id]'));

  const reorderedTickets = ticketNodes.map((node, index) => ({
    id: node.getAttribute('data-ticket-id'),
    priority: ticketNodes.length - index
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
function handleKanbanDragStart(e) {
  // Store the human ID of the ticket being dragged
  e.dataTransfer.setData("text/plain", e.currentTarget.dataset.ticketId);
  e.dataTransfer.effectAllowed = "move";
}

function handleKanbanDragOver(e) {
  e.preventDefault();
  e.dataTransfer.dropEffect = "move";

  // Highlight drop zone
  const dropZone = e.currentTarget.closest(".ticket-drop-zone");
  if (dropZone) {
    dropZone.classList.add("bg-stone-300/40", "dark:bg-onyx-600/40");
  }
}

function handleKanbanDragLeave(e) {
  const dropZone = e.currentTarget.closest(".ticket-drop-zone");
  if (dropZone) {
    dropZone.classList.remove("bg-stone-300/40", "dark:bg-onyx-600/40");
  }
}

async function handleKanbanDrop(e, project) {
  e.preventDefault();

  const dropZone = e.currentTarget.closest(".ticket-drop-zone");
  if (!dropZone) return;

  dropZone.classList.remove("bg-stone-300/40", "dark:bg-onyx-600/40");

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
