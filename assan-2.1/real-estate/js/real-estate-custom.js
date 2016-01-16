//sticky header

$(window).resize(function () {
    $(".navbar-collapse").css({maxHeight: $(window).height() - $(".navbar-header").height() + "px"});
});
//sticky header on scroll
$(document).ready(function () {
    $(window).load(function () {
        $(".sticky").sticky({topSpacing: 0});
    });
});



//slider revolution
jQuery(document).ready(function () {

    revapi = jQuery('.tp-banner').revolution(
            {
                delay: 6000,
                startwidth: 1170,
                startheight: 450,
                hideThumbs: 10,
                fullWidth: "on",
                forceFullWidth: "on",
                navigationStyle: "preview4"
            });

});	//ready

//all property slider
$(document).ready(function () {

    $("#property-slider").owlCarousel({
        autoPlay: 4000, //Set AutoPlay to 3 seconds

        items: 3,
        itemsDesktop: [1199, 3],
        itemsDesktopSmall: [979, 3],
        itemsTablet: [768, 2],
        itemsMobile: [479, 1],
        pagination: false,
        navigation: true,
        navigationText: ["<i class='fa fa-angle-left'>", "<i class='fa fa-angle-right'>"]
    });
});

//select box
(function () {
    [].slice.call(document.querySelectorAll('select.cs-select')).forEach(function (el) {
        new SelectFx(el);
    });
})();



//thumb slider
$(window).load(function() {
  // The slider being synced must be initialized first
  $('#carousel').flexslider({
    animation: "slide",
    controlNav: false,
    animationLoop: false,
    slideshow: false,
    itemWidth: 210,
    itemMargin: 5,
    asNavFor: '#slider'
  });
 
  $('#slider').flexslider({
    animation: "slide",
    controlNav: false,
    animationLoop: false,
    slideshow: false,
    sync: "#carousel"
  });
});


/***********************************************************
     * ACCORDION
     ***********************************************************/
    $('.panel-heading a[data-toggle="collapse"]').on('click', function () {
        if ($(this).closest('.panel-heading').hasClass('active')) {
            $(this).closest('.panel-heading').removeClass('active');
        } else {
            $('.panel-heading a[data-toggle="collapse"]').closest('.panel-heading').removeClass('active');
            $(this).closest('.panel-heading').addClass('active');
        }
    });
    
         /* ===================================================================
             TWEETIE -  TWITTER FEED PLUGIN THAT WORKS WITH NEW Twitter 1.1 API
             ==================================================================== */
            $('.tweet').twittie({
                apiPath: 'twit-api/tweet.php',
                dateFormat: '%b. %d, %Y',
                template: '{{tweet}} <div class="date">{{date}}</div> <a href="{{url}}"{{screen_name}}',
                count: 2
            });