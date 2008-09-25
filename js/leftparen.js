/*
  call_closure
  
  - url: the url of the web app
  - closure_key: comes from Scheme (e.g., using body-as-closure-key)
  - post_params: a dict from post keys to values.
  - callback (optional): a thunk
 */
function call_closure(url, closure_key, post_params, callback) {
  if (callback == null) {
    callback = function() { };
  }
  post_params['function'] = closure_key;
  $.post(url, post_params, callback, "json");
}

/*
  load_into_dom_id

  loads the result of calling the given closure into a dom elt on your web page.
  
  - url: the url of the web app (cannot have query params unless post_params is empty)
  - closure_key: comes from Scheme (e.g., using body-as-url)
  - dom_id: some elt in your page
  - post_params: a dict from post keys to values.
  
  MMM could add an optional fn to pass as 3rd arg to load, which is just a done callback
 */
function load_into_dom_id(url, closure_key, dom_id, post_params) {
  var elt_str = '#' + dom_id;
  post_params['function'] = closure_key;
  $(elt_str).load(url, post_params);
}

// turns the given link_elt to a show/hide toggler of the given elt.
function show_hide_link(elt_id, link_elt_id, hide_label, show_by_default) {
  $(document).ready(function() {
      var to_toggle = $('#' + elt_id);
      var link_elt = $('#' + link_elt_id);
      var show_label = link_elt.html();
      if (show_by_default) {
	link_elt.html(hide_label);
	to_toggle.show();
      }
      else {
	link_elt.html(show_label);
	to_toggle.hide();
      }
      link_elt.click (function() {
	  if (link_elt.html() == hide_label) {
	    link_elt.html(show_label);
	  }
	  else {
	    link_elt.html(hide_label);
	  }
	  to_toggle.toggle();
	  return false;
	});
    });
}
