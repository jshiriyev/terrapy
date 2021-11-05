function changeClass(){
  document.getElementById("myNAVlnk").addClass = "active";
}
window.onload = function(){
  document.getElementById("myNAVlnk").addEventListener('click',changeClass);
}
