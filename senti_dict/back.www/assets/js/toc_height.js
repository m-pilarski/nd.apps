    
function adjustTOCHeight() {
  var tocContainer = document.querySelector('#page-toc-container');
  if (!!tocContainer) {
    var tocBody = document.querySelector('#page-toc');
    if (!!tocBody) {
      tocContainer.style.height = tocBody.scrollHeight + 'px';
    }
  }
}

window.addEventListener('load', adjustTOCHeight);
window.addEventListener('resize', adjustTOCHeight);