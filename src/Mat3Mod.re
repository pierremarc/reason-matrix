open Common;
open Mat3;

let create = () => Mat3([|1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0|]);

let clone: mat3 => mat3 = a => Mat3(app(Array.map(v => v), a));

let fromValues = (m00, m01, m02, m10, m11, m12, m20, m21, m22) =>
  Mat3([|m00, m01, m02, m10, m11, m12, m20, m21, m22|]);

let identity = create;

let transpose =
  app(a => Mat3([|a[0], a[3], a[6], a[1], a[4], a[7], a[2], a[5], a[8]|]));

let invert =
  app(a => {
    let a00 = a[0];
    let a01 = a[1];
    let a02 = a[2];
    let a10 = a[3];
    let a11 = a[4];
    let a12 = a[5];
    let a20 = a[6];
    let a21 = a[7];
    let a22 = a[8];

    let b01 = a22 *. a11 -. a12 *. a21;
    let b11 = -. a22 *. a10 +. a12 *. a20;
    let b21 = a21 *. a10 -. a11 *. a20;

    switch (a00 *. b01 +. a01 *. b11 +. a02 *. b21) {
    | d when d === 0.0 => None
    | d =>
      let det = 1.0 /. d;
      Some(
        Mat3([|
          b01 *. det,
          (-. a22 *. a01 +. a02 *. a21) *. det,
          (a12 *. a01 -. a02 *. a11) *. det,
          b11 *. det,
          (a22 *. a00 -. a02 *. a20) *. det,
          (-. a12 *. a00 +. a02 *. a10) *. det,
          b21 *. det,
          (-. a21 *. a00 +. a01 *. a20) *. det,
          (a11 *. a00 -. a01 *. a10) *. det,
        |]),
      );
    };
  });

let adjoint =
  app(a => {
    let a00 = a[0];
    let a01 = a[1];
    let a02 = a[2];
    let a10 = a[3];
    let a11 = a[4];
    let a12 = a[5];
    let a20 = a[6];
    let a21 = a[7];
    let a22 = a[8];
    Mat3([|
      a11 *. a22 -. a12 *. a21,
      a02 *. a21 -. a01 *. a22,
      a01 *. a12 -. a02 *. a11,
      a12 *. a20 -. a10 *. a22,
      a00 *. a22 -. a02 *. a20,
      a02 *. a10 -. a00 *. a12,
      a10 *. a21 -. a11 *. a20,
      a01 *. a20 -. a00 *. a21,
      a00 *. a11 -. a01 *. a10,
    |]);
  });

let determinant =
  app(a => {
    let a00 = a[0];
    let a01 = a[1];
    let a02 = a[2];
    let a10 = a[3];
    let a11 = a[4];
    let a12 = a[5];
    let a20 = a[6];
    let a21 = a[7];
    let a22 = a[8];

    a00
    *. (a22 *. a11 -. a12 *. a21)
    +. a01
    *. (-. a22 *. a10 +. a12 *. a20)
    +. a02
    *. (a21 *. a10 -. a11 *. a20);
  });

let multiply =
  app2((a, b) => {
    let a00 = a[0];
    let a01 = a[1];
    let a02 = a[2];
    let a10 = a[3];
    let a11 = a[4];
    let a12 = a[5];
    let a20 = a[6];
    let a21 = a[7];
    let a22 = a[8];

    let b00 = b[0];
    let b01 = b[1];
    let b02 = b[2];
    let b10 = b[3];
    let b11 = b[4];
    let b12 = b[5];
    let b20 = b[6];
    let b21 = b[7];
    let b22 = b[8];
    Mat3([|
      b00 *. a00 +. b01 *. a10 +. b02 *. a20,
      b00 *. a01 +. b01 *. a11 +. b02 *. a21,
      b00 *. a02 +. b01 *. a12 +. b02 *. a22,
      b10 *. a00 +. b11 *. a10 +. b12 *. a20,
      b10 *. a01 +. b11 *. a11 +. b12 *. a21,
      b10 *. a02 +. b11 *. a12 +. b12 *. a22,
      b20 *. a00 +. b21 *. a10 +. b22 *. a20,
      b20 *. a01 +. b21 *. a11 +. b22 *. a21,
      b20 *. a02 +. b21 *. a12 +. b22 *. a22,
    |]);
  });

let translate: (vec2, mat3) => mat3 =
  (v, m) =>
    app(
      a => {
        let a00 = a[0];
        let a01 = a[1];
        let a02 = a[2];
        let a10 = a[3];
        let a11 = a[4];
        let a12 = a[5];
        let a20 = a[6];
        let a21 = a[7];
        let a22 = a[8];
        let x = fst(v);
        let y = snd(v);
        Mat3([|
          a00,
          a01,
          a02,
          a10,
          a11,
          a12,
          x *. a00 +. y *. a10 +. a20,
          x *. a01 +. y *. a11 +. a21,
          x *. a02 +. y *. a12 +. a22,
        |]);
      },
      m,
    );

let rotate: (float, mat3) => mat3 =
  (rad, m) =>
    app(
      a => {
        let a00 = a[0];
        let a01 = a[1];
        let a02 = a[2];
        let a10 = a[3];
        let a11 = a[4];
        let a12 = a[5];
        let a20 = a[6];
        let a21 = a[7];
        let a22 = a[8];

        let s = sin(rad);
        let c = cos(rad);

        Mat3([|
          c *. a00 +. s *. a10,
          c *. a01 +. s *. a11,
          c *. a02 +. s *. a12,
          c *. a10 -. s *. a00,
          c *. a11 -. s *. a01,
          c *. a12 -. s *. a02,
          a20,
          a21,
          a22,
        |]);
      },
      m,
    );

let scale: (vec2, mat3) => mat3 =
  (v, m) =>
    app(
      a => {
        let x = fst(v);
        let y = snd(v);
        Mat3([|
          x *. a[0],
          x *. a[1],
          x *. a[2],
          y *. a[3],
          y *. a[4],
          y *. a[5],
          a[6],
          a[7],
          a[8],
        |]);
      },
      m,
    );

let fromTranslation: vec2 => mat3 =
  v => Mat3([|1.0, 0.0, 0.0, 0.0, 1.0, 0.0, fst(v), snd(v), 1.0|]);

let fromRotation: float => mat3 =
  rad => {
    let s = sin(rad);
    let c = cos(rad);
    Mat3([|1.0, c, s, -. s, c, 0.0, 0.0, 0.0, 1.0|]);
  };

let fromScaling: vec2 => mat3 =
  v => Mat3([|fst(v), 0.0, 0.0, 0.0, snd(v), 0.0, 0.0, 0.0, 1.0|]);