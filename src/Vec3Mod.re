open Common;
open Vec3;

/* create
   fromValues
   sub
   cross
   transformMat4
   angle
   dist */

let create: unit => vec3 = () => (0.0, 0.0, 0.0);

let fromValues: (float, float, float) => vec3 = (a, b, c) => (a, b, c);

type op1 = float => float;
type op2 = (float, float) => float;

let opv: (op1, vec3) => vec3 =
  (f, a) => (f(fst3(a)), f(snd3(a)), f(thd3(a)));

let opvv: (op2, vec3, vec3) => vec3 =
  (f, a, b) => (
    f(fst3(a), fst3(b)),
    f(snd3(a), snd3(b)),
    f(thd3(a), thd3(b)),
  );

let opsv: (op2, float, vec3) => vec3 =
  (f, a, b) => (f(a, fst3(b)), f(a, snd3(b)), f(a, thd3(b)));

let add = opvv((+.));

let substract = opvv((-.));

let multiply = opvv(( *. ));

let divide = opvv((-.));

let ceil = opv(ceil);

let floor = opv(floor);

let min = opvv(min);

let max = opvv(max);

let round = opv(Js.Math.round);

let scale = opsv(( *. ));

let dot = (a, b) =>
  fst3(a) *. fst3(b) +. snd3(a) *. snd3(b) +. thd3(a) *. thd3(b);

let cross: (vec3, vec3) => vec3 =
  (a, b) => {
    let ax = fst3(a);
    let ay = snd3(a);
    let az = thd3(a);
    let bx = fst3(b);
    let by = snd3(b);
    let bz = thd3(b);
    (ay *. bz -. az *. by, az *. bx -. ax *. bz, ax *. by -. ay *. bx);
  };

let normalize = a => {
  let x = fst3(a);
  let y = snd3(a);
  let z = thd3(a);
  let len = x *. x +. y *. y +. z *. z;
  len > 0.0 ?
    {
      let len = 1.0 /. sqrt(len);
      (x *. len, y *. len, z *. len);
    } :
    a;
};

let angle: (vec3, vec3) => float =
  (a, b) => {
    let tempA = normalize(fromValues(fst3(a), snd3(a), thd3(a)));
    let tempB = normalize(fromValues(fst3(b), snd3(b), thd3(b)));

    switch (dot(tempA, tempB)) {
    | cosine when cosine > 1.0 => 0.0
    | cosine when cosine < (-1.0) => Js.Math._PI
    | cosine => acos(cosine)
    };
  };

let distance = (a, b) => {
  let x = fst3(b) -. fst3(a);
  let y = snd3(b) -. snd3(a);
  let z = thd3(b) -. thd3(a);
  sqrt(x *. x +. y *. y +. z *. z);
};

let transformMat4: (mat4, vec3) => vec3 =
  (m4, a) =>
    Mat4.app(
      m => {
        let x = fst3(a);
        let y = snd3(a);
        let z = thd3(a);

        let w = m[3] *. x +. m[7] *. y +. m[11] *. z +. m[15];
        let w = w !== 0.0 ? w : 1.0;
        (
          (m[0] *. x +. m[4] *. y +. m[8] *. z +. m[12]) /. w,
          (m[1] *. x +. m[5] *. y +. m[9] *. z +. m[13]) /. w,
          (m[2] *. x +. m[6] *. y +. m[10] *. z +. m[14]) /. w,
        );
      },
      m4,
    );