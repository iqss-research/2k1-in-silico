const myDefaultAllowList = bootstrap.Tooltip.Default.allowList
myDefaultAllowList.iframe = []
const myDefaultWhiteList = bootstrap.Tooltip.Default.whiteList
  myDefaultWhiteList.iframe = []

addTooltip_sanitize = function(id, type, opts) {
    var $id = shinyBS.getTooltipTarget(id);
    var dopts = {html: true, sanitize: false};
    opts = $.extend(opts, dopts);

    if(type == "tooltip") {
      $id.tooltip("destroy");
      $id.tooltip(opts);
    } else if(type == "popover") {
      $id.popover("destroy");
      $id.popover(opts);
    }
  };

$(document).ready(function() {
  const myDefaultAllowList = bootstrap.Tooltip.Default.allowList
  myDefaultAllowList.iframe = []
  const myDefaultWhiteList = bootstrap.Tooltip.Default.whiteList
  myDefaultWhiteList.iframe = []
  $('[data-bs-toggle="popover"').popover({
    allowList: myDefaultAllowList,
    html: true
  })
  $('[data-toggle="popover"]').popover({
    whiteList: myDefaultWhiteList
  })
})

$(document).ready(function() {
  addTooltip_sanitize = function(id, type, opts) {
    var $id = shinyBS.getTooltipTarget(id);
    var dopts = {html: true, sanitize: false};
    opts = $.extend(opts, dopts);

    if(type == "tooltip") {
      $id.tooltip("destroy");
      $id.tooltip(opts);
    } else if(type == "popover") {
      $id.popover("destroy");
      $id.popover(opts);
    }
  };
})
