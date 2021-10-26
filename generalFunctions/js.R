js1 <- "
$(document).on('change', '.dynamicSI .selector select', function(){
  Shiny.setInputValue('lastSelectId', this.id, {priority: 'event'});
});"