//luadraw 2.6
//options: +W640 +H576 -V +A +FN +UA
#version 3.7;
global_settings { charset utf8
     ambient_light rgb 1.5
     assumed_gamma 1.8}
background{ color srgbt<0,0,0,1> }
camera{ orthographic
     location 17320.508076*<-0.75,0.433013,0.5>
     sky <0.433013,-0.25,0.866025>
     up 9*y
     right 10*x
     look_at <0,0,0>
     translate <0.216506,-0.125,0.433013>}
light_source { 17320.508076*<-0.650765,0.015733,0.759117> color rgb<1,1,1>}

//declarations

#declare sph = sphere{
    <0,0,0> 4
  clipped_by { plane{ <0,0,-1>, 0 } }
  }

#declare cyl1 = cylinder{
    <0,2.666667,-1> <0,2.666667,4> 1.333333
  }

#declare cylext = union{
    object { cyl1 }
    object { cyl1 rotate 90*z }
  }

#declare cyl2 = cylinder{
    <0,2.666667,-1> <0,2.666667,4> 1.332333
  }

#declare object1 = difference{
    object { sph }
    object { cylext }
  }

#declare object2 = union{
    object { cyl2 }
    object { cyl2 rotate 90*z }
    clipped_by{ object{ sph} }
  }

#declare object3 =  union{
    cone{ <-4.732825,0,0> 0.089058
           <-5,0,0> 0 }
    cylinder{<5,0,0>,<-4.732825,0,0> 0.014058}
    cone{ <0,4.732825,0> 0.089058
           <0,5,0> 0 }
    cylinder{<0,-5,0>,<0,4.732825,0> 0.014058}
    cone{ <0,0,4.732825> 0.089058
           <0,0,5> 0 }
    cylinder{<0,0,-5>,<0,0,4.732825> 0.014058}
  }

//renderings

object{ object1
  texture{ 
         pigment{ color rgbt<0,0,1,0.3>}
         finish{ ambient 0.35 diffuse 0.8 phong 0.5} }
      }

object{ object2
  texture{ 
         pigment{ color rgbt<0.6784,0.8471,0.902,0.2>}
         finish{ ambient 0.35 diffuse 0.8 phong 0.5} }
      }

object{ object3
  texture{ 
         pigment{ color rgbt<1,0.8431,0,0>}
         finish{ ambient 0.35 diffuse 0.8 phong 0.5} }
      }

