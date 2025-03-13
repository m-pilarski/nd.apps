function adjustNavbarSpacer() {
  var navbarContainer = document.querySelector('#page-navbar');
  if (!!navbarContainer) {
    var navbarSpacer = document.querySelector('#page-navbar-spacer');
    if (!!navbarSpacer) {
    	navbarSpacer.style.height = navbarContainer.scrollHeight + 'px';
    }
  }
}

window.addEventListener('load', adjustNavbarSpacer);
window.addEventListener('resize', adjustNavbarSpacer);

/*
const navbarToggle = document.querySelector('#navbarSupportedContent') 
navbarToggle.addEventListener('shown.bs.collapse', adjustNavbarSpacer) 
navbarToggle.addEventListener('hidden.bs.collapse', adjustNavbarSpacer) 
*/
