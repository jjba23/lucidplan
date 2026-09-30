// ==================================

function allowDrop(event) {
  event.preventDefault();
  event.dataTransfer.dropEffect = 'move';

  const list = document.getElementById('lucidplan-tickets-list');
  const target = event.target.closest('[draggable="true"]');

  if (target && target !== draggedElement && list.contains(target)) {
    const rect = target.getBoundingClientRect();
    const next = (event.clientY - rect.top) / (rect.bottom - rect.top) > 0.5;
    list.insertBefore(draggedElement, next ? target.nextSibling : target);
  }
}

async function dropTicketsItem(event) {
  event.preventDefault();
  const list = document.getElementById('lucidplan-tickets-list');
  const projectId = list.getAttribute('data-project-id');
  const ticketNodes = Array.from(list.querySelectorAll('[data-ticket-id]'));

  // Priority mapping: top item gets highest priority, or index-based ranking
  const reorderedTickets = ticketNodes.map((node, index) => ({
    id: node.getAttribute('data-ticket-id'),
    priority: ticketNodes.length - index // 1-indexed descending priority
  }));

  try {
    const response = await fetch(`/tickets/${projectId}/reorder`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({ tickets: reorderedTickets })
    });

    if (!response.ok) {
      console.error('Failed to update ticket priorities');
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
