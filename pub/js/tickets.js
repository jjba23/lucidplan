
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
