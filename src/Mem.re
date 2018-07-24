/* mem.re */
/*
 type float32Array = array(float);
 [@bs.new] external createArray : int => float32Array = "Float32Array";

 type vpage =
   | Page2(int)
   | Page3(int)
   | Page4(int);

 type vmem =
   | Mem2(vpage, int)
   | Mem3(vpage, int)
   | Mem4(vpage, int);

 /* let a2 = Page2(2 * 1024)
    let a3 = Page3(3 * 1024)
    let a4 = Page4(4 * 1024) */

 let ps2 = ref([createArray(2 * 1024)]);
 let ps3 = ref([createArray(3 * 1024)]);
 let ps4 = ref([createArray(4 * 1024)]);

 let pages = (ps2, ps3, ps4); */