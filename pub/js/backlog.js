
function allowDrop(ev) {
  ev.preventDefault();
}

function dragBacklogItem(ev) {
  ev.dataTransfer.setData("text", ev.target.id);
}

function dropBacklogItem(ev) {
  ev.preventDefault();
  // console.log("ev.dataTransfer");
  // console.log(ev.dataTransfer);
  var data = ev.dataTransfer.getData("text");
  // console.log("ev.target");
  // console.log(ev.target);
  // console.log("ev.target.parentNode.parentNode");
  // console.log(ev.target.parentNode.parentNode);    
  //ev.target.parentNode.parentNode.appendChild(document.getElementById(data));
  document.getElementById('lucidplan-backlog-list').appendChild(document.getElementById(data));
}
