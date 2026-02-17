//luadraw 2.6
//options: +W640 +H640 -V +A +FN +UA
#version 3.7;
global_settings { charset utf8
     ambient_light rgb 1.5
     assumed_gamma 1.8}
background{ color srgbt<0,0,0,1> }
camera{ orthographic
     location 2449.489743*<-0.55667,-0.663414,0.5>
     sky <0.321394,0.383022,0.866025>
     up 2.75*y
     right 1.75*x
     look_at <0,0,0>
     translate <-0.197558,0.736887,0.757772>}
light_source { 2449.489743*<-0.643138,-0.453677,0.616887> color rgb<1,1,1>}

//declarations

#declare object1 =  isosurface{ 
   function{pow(x,3)+pow(y,3)-z}
    contained_by{ box{ <0,0,0> <1,1,2>} }
    open
    evaluate 0.666667, 1.5, 0.7
    matrix <-1,0,0,
            0,1,0,
            0,0,1,
            0,0,0>
  }

#declare object2 =  isosurface{ 
   function{z-pow(x*sqrt(1-y*y)+y*sqrt(1-x*x),3)}
    contained_by{ box{ <0,0,0> <0.999,0.999,2>} }
    open
    evaluate 0.666667, 1.5, 0.7
    matrix <-1,0,0,
            0,1,0,
            0,0,1,
            0,0,0>
  }

//renderings

object{ object1
  texture{ 
         pigment{ color rgbt<0.2745,0.5098,0.7059,0>}
         finish{ ambient 0.35 diffuse 0.8 phong 0.5} }
      }

object{ object2
  texture{ 
         pigment{ color rgbt<0.8627,0.0784,0.2353,0>}
         finish{ ambient 0.35 diffuse 0.8 phong 0.5} }
      }

