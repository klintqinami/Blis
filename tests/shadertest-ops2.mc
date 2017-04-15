@fragment void fshader(out vec3 color)
{
  if ( (true || true) == true &&
       (true || false) == true &&
       (false || true) == true &&
       (false || false)== false &&
       !false == true &&
       !true == false &&
       --42 == 42)
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}
