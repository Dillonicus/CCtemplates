$( document ).ready(function() {

    if ($(document).width() <= 1000) {
      $(".book").removeClass("with-summary");
    }
    // Shift nav in mobile when clicking the menu.
    $(document).on('click', ".toggle-sidebar", function() {
      $(".book").toggleClass("with-summary");
    });

    // ScrollSpy also requires that we use a Bootstrap nav component.
    $('#toc ul').first().addClass('nav');
    $('.book-body').scrollspy({target: '#toc'});

    $("body").removeClass("preload");
    
    // Remove any automatically generated styling in the body of the document.
    $('#content style').remove();
    $('img').attr('width', '');

    // Add download buttons
    $('#content table, #content img').after("<button class='download_button'>Download</button>");
    $(".download_button").on("click", extracter);

    // Format special characters for tables
    $('table').wrap('<div class="table_wrapper"></div>')
    $('td, tf, th').each(function () {
      $(this).html($(this).html().replace("\^\{", "<sup>").replace("\}", "</sup>"));
      $(this).html($(this).html().replace("\_\{", "<sub>").replace("\}", "</sub>"));
      $(this).html($(this).html().replace("**", "<b>").replace("**", "</b>"));
      $(this).html($(this).html().replace("~~", "<i>").replace("~~", "</i>"));
    });
});

const table_css_vars = `

/* Tables */

table {
  border-collapse: collapse;
  background-color: #FFFFFF;
  margin-left: auto;
  margin-right: auto;
}

tbody.gt_table_body {
    border-top: 1.5px black solid;
    border-bottom: 1.5px black solid;
}

thead.gt_header {
    border-bottom: 2px black solid;
}

thead.gt_header th {
    padding-bottom: 5px;
}

thead.gt_col_headings th {
    padding: 6px;
}

table.gt_table th, td{
  margin-bottom: 10pt;
}

/* GT tables */

.gt_table td, .gt_table tr {
    padding: 4px 8px;
}

#content p.gt_footnote {
    margin-bottom: 0;
    font-size: 80%;
}

.gt_center {
  text-align: center;
}
/* Data tables */

.datatables {
  margin: 2em 0em;
}

.dataTables_length,
.dataTables_filter,
.dataTables_info,
.dataTables_paginate {
  font-size: 90%;
}

.pagedtable-index-current {
  font-weight: bold;
}

#main a.anchor-section {
    color: #BBB;
}

`

const head = document.head;
const table_style = document.createElement("style");
table_style.id = "html-table-css-variables";
table_style.innerHTML = table_css_vars;
// head.appendChild(table_style);
