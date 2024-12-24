
function allowDrop(ev) {
  ev.preventDefault();
}

function dragBacklogItem(ev) {
  ev.dataTransfer.setData("text", ev.target.id);
}

function dropBacklogItem(ev) {
  ev.preventDefault();
  var data = ev.dataTransfer.getData("text");
  ev.target.appendChild(document.getElementById(data));
}
