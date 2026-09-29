
function allowDrop(ev) {
  ev.preventDefault();
}

function dragTicketsItem(ev) {
  ev.dataTransfer.setData("text", ev.target.id);
}

function dropTicketsItem(ev) {
  ev.preventDefault();
  // console.log("ev.dataTransfer");
  // console.log(ev.dataTransfer);
  var data = ev.dataTransfer.getData("text");
  console.log(data);
  // console.log("ev.target");
  // console.log(ev.target);
  // console.log("ev.target.parentNode.parentNode");
  // console.log(ev.target.parentNode.parentNode);
  //ev.target.parentNode.parentNode.appendChild(document.getElementById(data));
  document.getElementById('lucidplan-tickets-list').appendChild(document.getElementById(data));
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
