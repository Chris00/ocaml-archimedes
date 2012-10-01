(* MS Bitmap format.  This format was chosen instead of the simpler
   PPM one because it is natively supported by Windows (the plaform on
   which we would like not to mandate the installation of Cairo).

   Uses the explanations available at
   http://en.wikipedia.org/wiki/BMP_file_format about the file
   format.  *)

type color = int

let min (i:int) j = if i <= j then i else j

(* All integers must be written in little-endian format
   (i.e. least-significant byte first). *)
let output_int16 fh i =
  output_byte fh (i land 0xFF);  output_byte fh (i lsr 8)

let output_int32 fh i =
  if i = 0 then output_string fh "\000\000\000\000"
  else
    let i0 = i land 0xFF and i = i lsr 8 in
    let i1 = i land 0xFF and i = i lsr 8 in
    let i2 = i land 0xFF in
    let i3 = i lsr 8 in
    output_byte fh i0;  output_byte fh i1;
    output_byte fh i2;  output_byte fh i3

let output_rgb fh i =
  let i0 = i land 0xFF and i = i lsr 8 in
  let i1 = i land 0xFF in
  let i2 = i lsr 8 in
  output_byte fh i0;  output_byte fh i1;  output_byte fh i2


let write fname img =
  (* Use the first line to determine the image width.  Will pad other
     lines with white or truncate them if they do not have the correct
     length. *)
  let h = Array.length img in
  if h = 0 then failwith "Archimedes.Bmp.write: height of the image = 0";
  let w = Array.length img.(0) in
  if w = 0 then failwith "Archimedes.Bmp.write: width of the image = 0";
  let fh = open_out_bin fname in
  (* padding rows (3 bytes per pixel) *)
  let neg_padding = (3 * w) mod 4 in
  let n_padding, padding = if neg_padding = 0 then 0, ""
                           else if neg_padding = 1 then 3, "\000\000\000"
                           else if neg_padding = 2 then 2, "\000\000"
                           else (* neg_padding = 3 *) 1, "\000" in
  (* size = header + padded data *)
  let file_size = 54 + h * (3 * w + n_padding) in
  (* Bitmap file header (14 bytes) *)
  output_string fh "BM";
  output_int32 fh file_size;
  output_int32 fh 0;        (* reserved, application specific *)
  output_int32 fh 54;       (* offset of data, from beginning of file *)
  (* DIB header: BITMAPINFOHEADER used (40 bytes) *)
  output_int32 fh 40;       (* bitmap header size *)
  output_int32 fh w;        (* width of bitmap in pixels. *)
  output_int32 fh h;        (* height of bitmap in pixels. *)
  output_int16 fh 1;        (* number of planes in this bitmap. *)
  output_int16 fh 24;       (* bits per pixel *)
  output_int32 fh 0;        (* compression: none (BI_RGB) *)
  output_int32 fh 0;        (* bitmap data size (bytes); BI_RGB => 0 *)
  output_int32 fh 11811;    (* horizontal resolution (pixel/meter); ~300dpi *)
  output_int32 fh 11811;    (* vertical resolution (pixel/meter) *)
  output_int32 fh 0;        (* number of colors in palette *)
  output_int32 fh 0;        (* number of important colors: all *)
  (* Color table (palette)  24 bits/pixel => none *)
  (* Bitmap data *)
  for l = h - 1 downto 0 do
    let row = img.(l) in
    let wl = min w (Array.length row) in
    for c = 0 to wl - 1 do
      output_rgb fh row.(c)
    done;
    (* Pad with white color if the row was not long enough *)
    for w = wl to w - 1 do
      output_string fh "\xFF\xFF\xFF";
    done;
    (* Pad to reach a multiple of 4 bytes. *)
    output_string fh padding;
  done;
  close_out fh
