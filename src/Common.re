/* Common utilities */

let epsilon = 0.000001;

let random = Js.Math.random;

let degree = Js.Math._PI /. 180.0;

let toRadian = a => a *. degree;

let equals = (a, b) =>
  Js.(
    Math.abs_float(a -. b) <= epsilon
    *. Math.max_float(
         Math.max_float(1.0, Math.abs_float(a)),
         Math.abs_float(b),
       )
  );

type vec2 = (float, float);
type vec3 = (float, float, float);

module Vec3 = {
  let fst3: vec3 => float = ((v, _, _)) => v;
  let snd3: vec3 => float = ((_, v, _)) => v;
  let thd3: vec3 => float = ((_, _, v)) => v;
};

type mat2d =
  | Mat2d(array(float));
type mat3 =
  | Mat3(array(float));
type mat4 =
  | Mat4(array(float));

module Mat3 = {
  let app: (array(float) => 'a, mat3) => 'a =
    (fn, a) =>
      switch (a) {
      | Mat3([|a, b, c, d, e, f, g, h|]) => fn([|a, b, c, d, e, f, g, h|])
      | _ => fn([||])
      };

  let app2: ((array(float), array(float)) => 'a, mat3, mat3) => 'a =
    (fn, a, b) =>
      switch (a, b) {
      | (
          Mat3([|a, b, c, d, e, f, g, h|]),
          Mat3([|a2, b2, c2, d2, e2, f2, g2, h2|]),
        ) =>
        fn([|a, b, c, d, e, f, g, h|], [|a2, b2, c2, d2, e2, f2, g2, h2|])
      | _ => fn([||], [||])
      };
};

module Mat4 = {
  let app: (array(float) => 'a, mat4) => 'a =
    (fn, a) =>
      switch (a) {
      | Mat4([|a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p|]) =>
        fn([|a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p|])
      | _ => fn([||])
      };
};