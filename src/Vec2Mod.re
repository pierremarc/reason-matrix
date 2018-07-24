/* Vec2 */

open Common;

let create: unit => vec2 = () => (0.0, 0.0);

let clone: vec2 => vec2 = v => (fst(v), snd(v));

let fromValues: (float, float) => vec2 = (a, b) => (a, b);

type op1 = float => float;
type op2 = (float, float) => float;

let opv: (op1, vec2) => vec2 = (f, a) => (f(fst(a)), f(snd(a)));

let opvv: (op2, vec2, vec2) => vec2 =
  (f, a, b) => (f(fst(a), fst(b)), f(snd(a), snd(b)));

let opsv: (op2, float, vec2) => vec2 =
  (f, a, b) => (f(a, fst(b)), f(a, snd(b)));

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

let squareDistance = (a, b) => {
  let x = fst(b) -. fst(a);
  let y = snd(b) -. snd(a);
  x *. x +. y *. y;
};

let distance = (a, b) => sqrt(squareDistance(a, b));

let length = a => distance((0.0, 0.0), a);

let negate = a => (-. fst(a), -. snd(a));

let inverse = a => (1.0 /. fst(a), 1.0 /. snd(a));

let normalize = a => {
  let len = length(a);
  len > 0.0 ?
    {
      let len2 = 1.0 /. sqrt(len);
      (fst(a) *. len2, snd(a) *. len2);
    } :
    a;
};

let dot = (a, b) => fst(a) *. fst(b) +. snd(a) *. snd(b);

let cross: (vec2, vec2) => vec3 =
  (a, b) => (0.0, 0.0, fst(a) *. fst(b) -. snd(a) *. snd(b));

let lerp: (float, vec2, vec2) => vec2 =
  (t, a, b) => {
    let ax = fst(a);
    let ay = snd(a);
    (ax +. t *. (fst(b) -. ax), ay +. t *. (snd(b) -. ay));
  };

let rotate: (float, vec2, vec2) => vec2 =
  (c, origin, a) => {
    let p0 = fst(a) -. fst(origin);
    let p1 = snd(a) -. snd(origin);
    let sinC = sin(c);
    let cosC = cos(c);
    (
      p0 *. cosC -. p1 *. sinC +. fst(origin),
      p0 *. sinC +. p1 *. cosC +. snd(origin),
    );
  };

let angle = (a, b) => {
  let x1 = fst(a);
  let y1 = snd(a);
  let x2 = fst(b);
  let y2 = snd(b);
  let la = length(a);
  let lb = length(b);
  let len1 = la > 0.0 ? 1.0 /. sqrt(la) : la;
  let len2 = lb > 0.0 ? 1.0 /. sqrt(lb) : lb;

  switch ((x1 *. x2 +. y1 *. y2) *. len1 *. len2) {
  | cosine when cosine > 1.0 => 0.0
  | cosine when cosine < (-1.0) => Js.Math._PI
  | cosine => acos(cosine)
  };
};

let transformMat3: (mat3, vec2) => vec2 =
  (m3, a) =>
    Mat3.app(
      m => {
        let x = fst(a);
        let y = snd(a);
        (m[0] *. x +. m[3] *. y +. m[6], m[1] *. x +. m[4] *. y +. m[7]);
      },
      m3,
    );