


/*=========================*/
/*====main navigation hover dropdown====*/
/*==========================*/
jQuery(document).ready(function () {

    jQuery('.js-activated').dropdownHover({
        instantlyCloseOthers: false,
        delay: 0
    }).dropdown();

});

//main flex slider
$(window).load(function() {
    $('.main-flex-slider').flexslider({
        animation: "fade",
        controlNav: false
    });
  });

//thumb slider
 $(window).load(function() {
    $('.thumb-slider').flexslider({
        animation: "slide",
        controlNav: false
    });
  });


