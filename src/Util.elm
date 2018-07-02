module Util exposing (scaleAngle)


scaleAngle : (Float, Float) -> Float -> (Float, Float)
scaleAngle (x, y) scale =
    (x * scale, y * scale)
