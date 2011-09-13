include Tests_common

module A = Archimedes

let description = "Text in data coordinates should be fitted correctly."

let draw bk =
  let vp = A.init ~w ~h ~dirs bk in
  A.Axes.box vp;
  A.Viewport.text vp ~coord:`Data 4. 2. ~pos:A.Backend.CB "Test CB at (4, 2)";
  A.Viewport.text vp ~coord:`Data 2. 1. ~pos:A.Backend.LT "Test CB at (2, 1)";
  A.close vp
