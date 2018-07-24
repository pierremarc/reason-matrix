open Common;
open Mat4;

let create = () =>
  Mat4([|
    1.0,
    0.0,
    0.0,
    0.0,
    0.0,
    1.0,
    0.0,
    0.0,
    0.0,
    0.0,
    1.0,
    0.0,
    0.0,
    0.0,
    0.0,
    1.0,
  |]);

let clone: mat4 => mat4 = a => Mat4(app(Array.map(v => v), a));

let identity = create;

let fromRotation: (vec3, float) => option(mat4) =
  (axis, rad) => {
    let x1 = Vec3.fst3(axis);
    let y1 = Vec3.snd3(axis);
    let z1 = Vec3.thd3(axis);
    let len = sqrt(x1 *. x1 +. y1 *. y1 +. z1 *. z1);

    len < epsilon ?
      None :
      {
        let s = sin(rad);
        let c = cos(rad);
        let t = 1.0 -. c;
        let x = x1 *. 1.0 /. len;
        let y = y1 *. 1.0 /. len;
        let z = z1 *. 1.0 /. len;

        /* // Perform rotation-specific matrix multiplication */

        Some(
          Mat4([|
            x *. x *. t +. c,
            y *. x *. t +. z *. s,
            z *. x *. t -. y *. s,
            0.0,
            x *. y *. t -. z *. s,
            y *. y *. t +. c,
            z *. y *. t +. x *. s,
            0.0,
            x *. z *. t +. y *. s,
            y *. z *. t -. x *. s,
            z *. z *. t +. c,
            0.0,
            0.0,
            0.0,
            0.0,
            1.0,
          |]),
        );
      };
  };