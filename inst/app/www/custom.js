$(document).ready(function() {
  // Use event delegation for dynamically created elements
  $(document).on('click', '.card-header', function(e) {
    // Don't trigger if clicking on the actual collapse button
    if (!$(e.target).closest('.card-tools').length) {
      // Find the collapse button in this header and trigger click
      var collapseBtn = $(this).find('[data-card-widget="collapse"]');
      if (collapseBtn.length > 0) {
        collapseBtn.trigger('click');
      }
    }
  });
  
  // Function to apply styles to card headers (both existing and new ones)
  function styleCardHeaders() {
    $('.card-header').css('cursor', 'pointer');
    $('.card-tools').css('cursor', 'default');
  }
  
  // Apply styles initially
  styleCardHeaders();
  
  // Watch for new elements and apply styles
  var observer = new MutationObserver(function(mutations) {
    mutations.forEach(function(mutation) {
      if (mutation.addedNodes.length > 0) {
        styleCardHeaders();
      }
    });
  });
  
  // Start observing
  observer.observe(document.body, {
    childList: true,
    subtree: true
  });

  // When the results box is maximized/restored, DataTables that were initialised
  // while their tab was hidden render with collapsed columns. Re-adjust every
  // DataTable in the card after the layout settles so the quantile header (and
  // the OCS/Mate tables) paint at their correct size.
  $(document).on('click', '[data-card-widget="maximize"]', function() {
    var card = $(this).closest('.card');
    setTimeout(function() {
      card.find('table.dataTable').each(function() {
        if ($.fn.dataTable.isDataTable(this)) {
          $(this).DataTable().columns.adjust();
        }
      });
    }, 400);
  });

  // Your existing tab script
  $('#cnv_1-sample_select_tabs li.active > a').addClass('active');

  $(document).on('shown.bs.tab', '#cnv_1-sample_select_tabs a[data-toggle="tab"]', function(e) {
    $('#cnv_1-sample_select_tabs a[data-toggle="tab"]').removeClass('active');
    $(e.target).addClass('active');
  });
});