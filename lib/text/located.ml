type 'a located = {
  at : Span.t;
  v : 'a
}

let located loc v = { at = Span.of_loc loc; v }
