-- luadraw_spherical.lua 
-- date 2026/06/13
-- version 3.2
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   https://www.ctan.org/license/lppl

local ld = luadraw
local pt3d = ld.pt3d
local Origin, vecI, vecJ, vecK, M, Ms = pt3d.Origin, pt3d.vecI, pt3d.vecJ, pt3d.vecK, pt3d.M, pt3d.Ms
local graph3d = ld.graph3d

local insidelabelcolor = "gray"
local hiddendelayed = false
local arrowBstyle = "->"
local arrowAstyle = "<-"
local arrowABstyle = "<->"

local sphere = {["C"]=Origin, ["R"]=3, 
    ["color"]="orange",
    ["opacity"]=1,
    ["mode"]=ld.mBorder, 
    ["edgecolor"]="lightgray", 
    ["edgestyle"] = "solid",
    ["edgewidth"] = "4",
    ["hiddenstyle"] = ld.Hiddenlinestyle,
    ["hiddencolor"] = "gray",
    ["show"] = true,
    ["horizon"] = nil
    } -- sphere definition
    
local before_sphere = {}
local after_sphere = {}
local hidden_part = {}

function graph3d:Clear_spherical()
    before_sphere = {}
    after_sphere = {}
    hidden_part = {}
    sphere = {["C"]=Origin, ["R"]=3, 
    ["color"]="orange",
    ["opacity"]=1,
    ["mode"]=ld.mBorder, 
    ["edgecolor"]="lightgray", 
    ["edgestyle"] = "solid",
    ["edgewidth"] = "4",
    ["hiddenstyle"] = ld.Hiddenlinestyle,
    ["hiddencolor"] = "gray",
    ["show"] = true,
    ["horizon"] = nil
    } -- sphere definition
    insidelabelcolor = "gray"
    arrowBstyle = "->"
    arrowAstyle = "<-"
    arrowABstyle = "<->"
end

function graph3d:Define_sphere( args )
    args = args or {}
    if args.center ~= nil then sphere.C = args.center end
    if args.radius ~= nil then sphere.R = args.radius end
    local Ct, R = sphere.C, sphere.R
    if args.color ~= nil then  sphere.color = args.color end
    if args.opacity ~= nil then sphere.opacity = args.opacity end
    if args.mode ~= nil then sphere.mode = args.mode end
    if args.edgecolor ~= nil then sphere.edgecolor = args.edgecolor end
    if args.edgestyle ~= nil then sphere.edgestyle = args.edgestyle end
    if args.hiddenstyle ~= nil then sphere.hiddenstyle = args.hiddenstyle end
    if args.hiddencolor ~= nil then sphere.hiddencolor = args.hiddencolor end
    if args.edgewidth ~= nil then sphere.edgewidth = args.edgewidth end
    if args.show ~= nil then sphere.show = args.show end
    insidelabelcolor = args.insidelabelcolor or "gray"
    arrowBstyle = args.arrowBstyle or "->"
    arrowAstyle = args.arrowAstyle or "<-"
    arrowABstyle = args.arrowABstyle or "<->"
    local mat = self.matrix3d
    local N, cam = self.Normal, ld.camera
    if not ld.isID3d(mat) then
        mat = ld.invmatrix3d(mat)
        N = ld.mLtransform3d(N,mat); cam = ld.mtransform3d(cam,mat)
    end
    if ld.projection_mode == "central" then
        sphere.horizon = { ld.interSS({Ct,R}, {(Ct+cam)/2,pt3d.abs(Ct-cam)/2}) }
    else
        sphere.horizon = {Ct, R, N}
    end
end

---------------- new functions -------------------------------------

function ld.sM(x,y,z) -- or sM(theta,phi) (theta, phi degrees), define a spherical dot in (C,vecI,vecJ,vecK)
    local C, R = sphere.C, sphere.R
    if z ~= nil then --cartesian coordinates
        return C + R*pt3d.normalize(M(x,y,z))
    else -- spherical coordinates
        return C + Ms(R,x*ld.deg,y*ld.deg)
    end
end

function ld.toSphere(A)
    local C, R = sphere.C, sphere.R
    local u = A-C
    if pt3d.N1(u)< 1e-12 then return end
    return C+R*pt3d.normalize(u)
end

local visibledot = function(A)
    if sphere.horizon == nil then print("Please define sphere first"); return end
    local I, r, n = table.unpack(sphere.horizon)
    return pt3d.dot(A-I,n) >= 0
end

function ld.interSphericalC(P1, P2)
    local C, R = sphere.C, sphere.R
    local D = ld.interPP(P1,P2)
    if D ~= nil then
        local I1, I2 = ld.interDS(D, {C,R})
        if (I1 ~= nil) and (I2 ~= nil) and (not visibledot(I1)) then
            I1, I2 = I2, I1
        end
        return I1, I2
    end
end

function ld.interGreatC(AB, CD)
-- AB = {A,B} (two points of sphere)
-- CD = {C,D} (two points of sphere)
-- options = {style=, color=, width=, opacity=}
    local C, R = sphere.C, sphere.R
    local A1, B1 = table.unpack(AB)
    local A2, B2 = table.unpack(CD)
    return ld.interSphericalC( ld.plane(A1,B1,C), ld.plane(A2,B2,C) )
end

function ld.projstereo_Sfacet(L, N, h, close) -- stereographic projection of a spherical facet
-- L is a list of 3D points on the sphere, 
-- the "segments" joining two consecutive points are arcs of a great circle (spherical facet).
    local C, R = sphere.C, sphere.R
    
    local function projstereo_Sarc(AB) 
    -- AB is a list of two 3D points on the sphere (not aligned with the center of the sphere) 
        local A, B = table.unpack(AB)
        if pt3d.N1(B-A) < 1e-12 then return end
        local inverse = false
        local d1, d2 = pt3d.N1(N-A), pt3d.N1(N-B)
        if d2 < 1e-12 then A,B = B,A; d1,d2 = d2,d1; inverse = true end 
        if d1 < 1e-12 then -- A is the pole
            A = ld.toSphere( (9*N+B)/10) 
            if inverse then A,B = B,A end
            return { ld.projstereo(A, {C,R}, N, h),"m", ld.projstereo(B, {C,R}, N, h), "l"}, true
        else -- A and B are not at the pole
            --local arc = ld.arc3d(A,C,B,R,1)[1]
            local a, b = table.unpack( ld.projstereo({A,B}, {C,R}, N, h) )
            local D = ld.toSphere((A+B)/2) --arc[#arc//2]
            if math.abs( pt3d.det(A-C,B-C,N-C) ) < 1e-8 then -- pole sur le grand cercle
                if pt3d.angle(N-C,D-C,1e-6) <= pt3d.angle(A-C,B-C,1e-6)/2 then  -- N entre A et B
                    local u = pt3d.normalize(b-a)
                    return {a, a-100*u, "l", b+100*u,"m", b, "l"}
                else
                    return {a,b,"l"}
                end
            else
                local c = ld.projstereo(D, {C,R}, N, h)
                local I, r, n = ld.circumcircle3d(a, b, c)
                local sens
                if (pt3d.det(a-I,c-I,n)>0) then sens = 1 else sens = -1 end
                return ld.arc3db(a,I,b,r,sens,n) -- path
            end
        end
    end

    if close == nil then close = true end
    local B, A, D = ld.toSphere(L[1])
    local n, p = #L
    local ret= {}
    if close then p = n+1 else p = n end
    for k = 2, p do
        A = B; B = ld.toSphere( L[1+(k-1)%n] )
        local apath, move = projstereo_Sarc({A,B}, N, h)
        if apath ~= nil then
            if (k>2) and (not move)  then table.remove(apath,1) end
            ld.insert(ret, apath)
        end
    end
    return ret
end


function ld.projstereo_Scircle(P, N, h) -- stereographic projection of a spherical circle
-- P = {A,n} is a plane
    local A, n = table.unpack(P)
    local C, R = sphere.C, sphere.R
    local I, r, v = ld.interPS(P,{C,R})
    if I == nil then return end
    if math.abs(pt3d.dot( N-A,n)) < 1e-8 then -- N is on the circle
        local B, D = ld.rotate3d( N, -10, {I,n}), ld.rotate3d(N, 10, {I,n})
        return {ld.projstereo(B, {C,R}, N, h), ld.projstereo(D, {C,R}, N, h), "l"}
    else
        local w = pt3d.prod(n, M(1,0,0))
        if pt3d.N1(w) < 1e-8 then w = pt3d.prod(n, M(0,1,0)) end
        w = pt3d.normalize(w)
        local B, D = I+r*w, I-r*w
        local E = ld.rotate3d(B, 90, {I,n})
        local b, d, e = table.unpack( ld.projstereo({B,D,E}, {C,R}, N, h) )
        return ld.circle3db( ld.circumcircle3d(b,d,e) )
    end
end

----------------------- new methods ---------------------------------

function graph3d:Dspherical()
    local oldlinestyle = self.param.linestyle
    local oldlinecolor = self.param.linecolor
    local oldlinewidth = self.param.linewidth
    local oldlineopacity = self.param.lineopacity
    local oldfillstyle = self.param.fillstyle
    local oldfillcolor = self.param.fillcolor
    local oldfillopacity = self.param.fillopacity

    local display_elt = function(elt) 
    -- elt={path,style,color,width,opacity,arrows} ou
    -- elt={text,anchor,options} (labels)
        if type(elt[1]) == "string" then -- a label
            self:Dlabel3d(elt[1],elt[2],elt[3])
        elseif pt3d.isPoint3d(elt[1]) then -- a dot
            self:Lineoptions(oldlinestyle, oldlinecolor, oldlinewidth); self:Lineopacity(oldlineopacity)
            self:Filloptions(oldfillstyle,oldfillcolor,oldfillopacity)
            self:Ddots3d(elt[1],elt[2])
        else
            self:Lineoptions(elt[2],elt[3],elt[4]); self:Lineopacity(elt[5])
            local arrowstyle, arrows = "", elt[6]
            
            if arrows == 1 then arrowstyle = arrowBstyle
            elseif arrows == 2 then arrowstyle = arrowABstyle
            elseif arrows == -1 then arrowstyle = arrowAstyle
            else arrows = 0
            end
            if (elt[7] ~= nil) and (elt[7] ~= "") then -- fill
                --self:Filloptions("gradient","ball color="..elt[7],elt[8])
                self:Filloptions("full",elt[7],elt[8])
            else self:Filloptions("none",nil,1)
            end
            self:Linecap("round"); -- pour que les liaisons soient correctes entre segments successifs
            if arrows ~= 0 then
                self:Dpath3d(elt[1],"arrows="..arrowstyle)
            else self:Dpath3d(elt[1])
            end
        end
    end

    for _, elt in ipairs(before_sphere) do
        display_elt(elt)
    end
    if sphere.show then
        self:Lineoptions(oldlinestyle, oldlinecolor, oldlinewidth); self:Lineopacity(oldlineopacity)
        self:Dsphere(sphere.C, sphere.R, {mode=sphere.mode, color=sphere.color, opacity=sphere.opacity,
            edgecolor=sphere.edgecolor, edgewidth=sphere.edgewidth, edgestyle=sphere.edgestyle, hiddenstyle=sphere.hiddenstyle, hiddencolor=sphere.hiddencolor})
    end
    for _, elt in ipairs(after_sphere) do
        display_elt(elt)
    end
    if hiddendelayed then self:Begindeferred() end
        if sphere.show and ld.Hiddenlines and (sphere.edgestyle ~= "noline") and (sphere.hiddenstyle ~= "noline") then
            self:Dsphere(sphere.C, sphere.R, {mode=ld.mBorder,edgecolor=sphere.edgecolor, edgewidth=3*sphere.edgewidth/4, edgestyle=sphere.hiddenstyle}) 
        end
        for _, elt in ipairs(hidden_part) do
            display_elt(elt)
        end    
    if hiddendelayed then self:Enddeferred() end
    self:Lineoptions(oldlinestyle, oldlinecolor, oldlinewidth); self:Lineopacity(oldlineopacity)
    self:Filloptions(oldfillstyle,oldfillcolor,oldfillopacity)
    self:Clear_spherical()
end

-- ajouter un cercle tracé sur la sphère
function graph3d:DScircle(P,options) -- P={A,u} (plane)
-- options = {style=, color=, width=, opacity=, out=}
    options = options or {}
    local style = options.style or self.param.linestyle
    local color = options.color or self.param.linecolor
    local width = options.width or self.param.linewidth
    local opacity = options.opacity or self.param.lineopacity
    local hidden = options.hidden
    if hidden == nil then hidden = ld.Hiddenlines end
    local out = options.out -- returns end points of hidden part if it exists
    
    local C, R = sphere.C, sphere.R
    local N, angle = self.Normal
    
    local acircle = function(I,r,v,u) -- when we have to draw a circle
        local w = pt3d.prod(u,vecI)
        if pt3d.N1(w) < 1e-12 then w =pt3d.prod(u,vecJ) end
        local J = I+r*pt3d.normalize(w) -- a point of the circle
        if  visibledot(I) then --(pt3d.dot(v,N) > 0) then -- visible
            table.insert(after_sphere, {{J,I,u,"c"},style,color,width,opacity})
            if hidden and hiddendelayed and (style ~= "noline") then
                table.insert(hidden_part, {{J,I,u,"c"},ld.Hiddenlinestyle,color,width,opacity})
            end
        elseif hidden and (style ~= "noline") then
            table.insert(hidden_part, {{J,I,u,"c"},ld.Hiddenlinestyle,color,width,opacity})
        else 
            --self:Beginadvanced()
            table.insert(before_sphere, {{J,I,u,"c"},style,color,width,opacity})
            --self:Endadvanced()
        end
    end
    local A, b, c, u
    if #P == 3 then
        A, b, c = table.unpack(P)
        A= ld.toSphere(A); b = ld.toSphere(b); c = ld.toSphere(c)
        u = pt3d.prod(b-A,c-A)
        P = {A,u}
    else
        A, u = table.unpack(P)
    end
    local I = ld.proj3d(C,P) -- center 
    if ld.projection_mode == "central" then N = ld.camera-I end
    local mat = self.matrix3d
    if not ld.isID3d(mat) then mat = ld.invmatrix3d(mat); N = ld.mLtransform3d(N,mat) end
    if pt3d.dot(u,I-C) < 0 then u = -u end
    local d = pt3d.abs(C-I)
    if d >= R then  return end -- no circle
    local v, r = I-C
    if pt3d.N1(v) < 1e-12 then -- C is on P (big circle)
        v = Origin; r = R; --angle = 0
    else
        r = math.sqrt(R^2- pt3d.abs2(v))
    end
    if pt3d.N1(pt3d.prod(u,N))< 1e-12 then -- P has the same direction than screen 
        acircle(I,r,v,u)
    else
        local n2 = pt3d.normalize(pt3d.prod(N,u))
        local n1 = pt3d.normalize(pt3d.prod(u,n2))
        if ld.projection_mode == "central" then 
            angle = (r*r-pt3d.dot(v,N))/(r*pt3d.dot(n1,N))
        else
            angle = -pt3d.dot(v,N)/(r*pt3d.dot(n1,N)) 
        end
        if math.abs(angle) > 1+1e-6 then acircle(I,r,v,u,angle)
        elseif (1<angle) and (angle<1+1e-6) then angle = 1 
        elseif (angle <-1) and (1-1e-6<angle) then angle = -1 
        else
            local t0 = math.acos(angle)
            if math.abs(t0) < 1e-6 then acircle(I,r,v,u)
            else
                local A, B = I+r*math.cos(t0)*n1+r*math.sin(t0)*n2, I+r*math.cos(t0)*n1-r*math.sin(t0)*n2
                if out ~= nil then
                    ld.insert(out,{A,B})
                end
                local sens = 1
                if pt3d.dot(I-C+r*n1,N) < 0 then 
                    sens = -1
                end
                table.insert(after_sphere, {{A,I,B,r,sens,u,"ca"},style,color,width,opacity})
                if hidden and hiddendelayed and (style ~= "noline") then
                    table.insert(hidden_part, {{A,I,B,r,sens,u,"ca"},ld.Hiddenlinestyle,color,width,opacity})
                end
                if hidden  and (style ~= "noline") then 
                    table.insert(hidden_part, {{A,I,B,r,-sens,u,"ca"},ld.Hiddenlinestyle,color,width,opacity})
                else
                    --self:Beginadvanced()
                    table.insert(before_sphere, {{A,I,B,r,-sens,u,"ca"},style,color,width,opacity})
                    --self:Endadvanced()
                end
            end
        end
    end
end

function graph3d:DSbigcircle(AB,options) -- AB = {A,B} (two points of sphere)
-- options = {style=, color=, width=, opacity=}
    local C, R = sphere.C, sphere.R
    local A, B = table.unpack(AB)
    A = ld.toSphere(A); B = ld.toSphere(B)
    local P = {A, pt3d.prod(A-C, B-C)}
    self:DScircle(P,options)
end    

-- ajouter un segment dans la scène
function graph3d:DSseg(seg,options) -- seg={A,B} (segment)
-- options = {style=, color=, width=, opacity=, arrows=}
    local C, R = sphere.C, sphere.R
    local A, B = table.unpack(seg)
    if A == B then return end
    options = options or {}
    local style = options.style or self.param.linestyle
    local color = options.color or self.param.linecolor
    local width = options.width or self.param.linewidth
    local opacity = options.opacity or self.param.lineopacity
    local hidden = options.hidden
    if hidden == nil then hidden = ld.Hiddenlines end
    local arrows = options.arrows or 0 --0/1/2
    local out = options.out -- intersections points between seg and sphere
    local arrowA, arrowB = 0, 0
    if arrows == 1 then arrowB = 1
    elseif arrows == 2 then arrowA = 1; arrowB = 1
    end
    local I, r, n = table.unpack( sphere.horizon )
    
    local add_seg_in = function(U,V,arrow) -- [U,V] is inside the sphere
        table.insert(before_sphere, {{U,V,"l"},style,color,width,opacity,arrow} )
    end
    
    local add_seg_out = function(U,V,arrow)
        local dev, der = ld.splitseg({U,V},{I,n})
        local arrowder, arrowdev = 0, 0
        if #der ~= 0 then 
            if (arrow == 0) or (#dev == 0) then
                arrowder = arrow
            else
                if arrow == 2 then
                    if pt3d.dot(V-U,n) < 0 then
                        arrowder = 1; arrowdev = 1
                        dev = ld.reverse(dev)
                    else
                        arrowder = 1; arrowdev = 1
                        der = ld.reverse(der)
                    end
                else -- arrow =1 en V
                    if pt3d.dot(V-U,n) < 0 then
                        arrowder = 1; arrowdev = 0
                    else
                        arrowder = 0; arrowdev = 1
                    end
                end
            end
            table.insert(der,"l"); 
            table.insert(before_sphere, {der,style,color,width,opacity,arrowder} )
        else arrowdev = arrow
        end
        if #dev ~= 0 then 
            table.insert(dev,"l")
            table.insert(after_sphere, {dev,style,color,width,opacity,arrowdev}) 
            if hidden and hiddendelayed and (style ~= "noline") then
                --table.insert(hidden_part, {dev,ld.Hiddenlinestyle,color,width,opacity,arrowdev}) 
            end
        end
    end

    if hidden and (style ~= "noline") then 
        table.insert(hidden_part, {{A,B,"l"},ld.Hiddenlinestyle,color,width,opacity,arrows})  -- whole seg
    else -- pas de lignes cachées
        --self:Beginadvanced()
        --table.insert(before_sphere, {{A,B,"l"},style,color,width,opacity,arrows})
        --self:Endadvanced()
    end
    local I = ld.dproj3d(C,{A,B-A})
    if pt3d.abs(C-I) >= (R-1e-12) then 
        add_seg_out(A,B,arrows) -- no intersection with sphere
    else
        local u, v, ell = pt3d.normalize(B-A), A-C, pt3d.abs(B-A)
        local b, c = pt3d.dot(u,v), pt3d.abs2(v)-R^2
        local D = math.sqrt(b^2-c)
        local t1, t2 = -b-D, -b+D 
        local J, K = A+t1*u, A+t2*u -- J and K are on S
        if (t1 >= ell) or (t2 <= 0) then
            add_seg_out(A,B,arrows) -- [A,B] is out S
        elseif (t1 <= 0) and (t2 >= ell) then
            -- [A,B] is in S
            add_seg_in(A,B,arrows)
        elseif (t1 > 0) and (t1<=ell) and (t2>=ell) then
            add_seg_out(J,A,arrowA) -- [A,J] is out S, [J,B] is in S
            add_seg_in(J,B,arrowB)
            if out ~= nil then table.insert(out,J) end
        elseif (t1 > 0)  and (t2 < ell) then
            add_seg_out(J,A,arrowA) -- [A,J] is out S, -- [J,K] is in S
            add_seg_in(J,K,0)
            add_seg_out(K,B,arrowB) -- [K,B] is out S
            if out ~= nil then ld.insert(out,{J,K}) end
        elseif (t1 <= 0) and (t2 < ell) then 
            add_seg_out(K,B,arrowB) -- [A,K] is in S, [K,B] is out S
            add_seg_in(K,A,arrowA)
            if out ~= nil then table.insert(out,K) end
        end
    end
end


function graph3d:DSline(d,options) -- d = {A,u} a line
-- options = {style=, color=, width=, opacity=, arrows=, scale= }
    options = options or {}
    options.scale = options.scale or 1
    local L = self:Line3d2seg(d,options.scale)
    self:DSseg(L,options)
end


function graph3d:DSpolyline(L,options) -- L = 3d polyline
-- options = {style=, color=, width=, opacity=, arrows=, close=}
    options = options or {}
    options.style = options.style or self.param.linestyle
    options.color = options.color or self.param.linecolor
    options.width = options.width or self.param.linewidth
    options.opacity = options.opacity or self.param.lineopacity    
    local hidden = options.hidden
    if hidden == nil then hidden = ld.Hiddenlines end
    options.arrows = options.arrows or 0 --0/1/2
    local arrows = options.arrows
    local close = options.close or false
    if pt3d.isPoint3d(L[1]) then L = {L} end
    local oldstyle = ld.Hiddenlinestyle
    ld.Hiddenlinestyle = "noline"
    for _, cp in ipairs(L) do
        local B, A = cp[1] 
        if hidden  and (options.style ~= "noline") then 
            local ends = {"l"}
            if close then table.insert(ends,"cl") end
            table.insert(hidden_part, {ld.concat(cp,ends),oldstyle,options.color,options.width,options.opacity,options.arrows})  -- whole polyline
        end
        local n, p = #cp
        if close then p = n+1 else p = n end
        for k = 2, p do
            A = B; B = cp[(k-1)%n+1]
            options.arrows = 0
            if (arrows == 2) then
                if (k == 2) and (k == n) then options.arrows = 2
                elseif (k == 2) then options.arrows = -1
                elseif (k == n) then options.arrows = 1
                end 
            elseif (arrows == 1) then
                if (k == n) then options.arrows = 1 end
            end
            if pt3d.abs(B-A) > 1e-10 then
                self:DSseg({A,B},options)
            end
        end
    end
    ld.Hiddenlinestyle = oldstyle
end

-- ajouter un arc de grand cercle sur la sphère
function graph3d:DSarc(AB,sens,options)
-- options = {style=, color=, width=, opacity=, arrows=, normal=}
    options = options or {}
    local A, B = table.unpack(AB)
    if pt3d.abs(A-B)<1e-12 then return end
    local style = options.style or self.param.linestyle
    local color = options.color or self.param.linecolor
    local width = options.width or self.param.linewidth
    local opacity = options.opacity or self.param.lineopacity
    local hidden = options.hidden
    if hidden == nil then hidden = ld.Hiddenlines end
    local arrows = options.arrows or 0 --0/1/2
    local normal = options.normal or vecK
    local arrowA, arrowB = 0, 0
    if arrows == 1 then arrowB = 1
    elseif arrows == 2 then arrowA = 1; arrowB = 1
    end
    local C, R = sphere.C, sphere.R
    local N = self.Normal
    local cam = ld.camera
    if ld.projection_mode == "central" then N = ld.camera-C end
    local mat = self.matrix3d
    if not ld.isID3d(mat) then 
        mat = ld.invmatrix3d(mat); N = ld.mLtransform3d(N,mat); cam = ld.mtransform3d(ld.camera,mat) 
    end
    local A, B = ld.toSphere(A), ld.toSphere(B) -- to have points on sphere
    local u = pt3d.prod(A-C,B-C)
    if pt3d.N1(u) < 1e-12 then  -- points alignés avec le centre !
        if normal ~= nil then 
            u = normal 
        else
            u = pt3d.prod(B-A,vecI)
            if pt3d.N1(u) < 1e-12 then u = pt3d.prod(B-A,vecJ) end
        end
    end 
    if (ld.projection_mode ~= "central") and (pt3d.N1(pt3d.prod(u,N))< 1e-12) then -- P est le plan de l'écran
        table.insert(after_sphere, {{A,C,B,R,sens,u,"ca"},style,color,width,opacity,arrows} )
        if hidden and hiddendelayed and (style ~= "noline") then
            table.insert(hidden_part, {{A,C,B,R,sens,u,"ca"},ld.Hiddenlinestyle,color,width,opacity})
        end
    else
        local M1, M2
        if ld.projection_mode == "central" then
            M2, M1 = ld.interCS({C,R,u}, {(C+cam)/2, pt3d.abs(C-cam)/2} )
            if (M2 ~= nil) and (M1 ~= nil) and (pt3d.det(cam-C,u,M1-C) < 0) then 
                M1, M2 = M2, M1 
            end
        else
            local n2 = pt3d.normalize(pt3d.prod(N,u))
            local n1 = pt3d.normalize(pt3d.prod(n2,u))
            M1, M2 = C+R*n2, C-R*n2
        end
        if visibledot(A) and visibledot(B) then -- A et B sont visibles
            if sens == 1 then
                table.insert(after_sphere, {{A,C,B,R,sens,u,"ca"},style,color,width,opacity,arrowB})
                if hidden and hiddendelayed and (style ~= "noline") then
                    table.insert(hidden_part, {{A,C,B,R,sens,u,"ca"},ld.Hiddenlinestyle,color,width,opacity})
                end
            else
                table.insert(after_sphere, {{A,C,M1,R,sens,u,"ca"},style,color,width,opacity,-arrowA})
                table.insert(after_sphere, {{M2,C,B,R,sens,u,"ca"},style,color,width,opacity,arrowB})
                if hidden and hiddendelayed and (style ~= "noline") then
                    table.insert(hidden_part, {{A,C,M1,R,sens,u,"ca"},ld.Hiddenlinestyle,color,width,opacity})
                    table.insert(hidden_part, {{M2,C,B,R,sens,u,"ca"},ld.Hiddenlinestyle,color,width,opacity})
                end
                if hidden  and (style ~= "noline") then
                    table.insert(hidden_part, {{M1,C,M2,R,sens,u,"ca"},ld.Hiddenlinestyle,color,width,opacity,0})
                else
                    --self:Beginadvanced()
                    table.insert(before_sphere, {{M1,C,M2,R,sens,u,"ca"},style,color,width,opacity,0})
                    --self:Endadvanced()
                end
            end
        elseif (not visibledot(A)) and (not visibledot(B)) then -- A et B sont cachés
            if sens == 1 then
                if hidden  and (style ~= "noline") then
                    table.insert(hidden_part, {{A,C,B,R,sens,u,"ca"},ld.Hiddenlinestyle,color,width,opacity,-arrowA})
                else
                    --self:Beginadvanced()
                    table.insert(before_sphere, {{A,C,B,R,sens,u,"ca"},style,color,width,opacity,arrows})
                    --self:Endadvanced()
                end
            else
                table.insert(after_sphere, {{M2,C,M1,R,sens,u,"ca"},style,color,width,opacity,0})
                if hidden and hiddendelayed and (style ~= "noline") then
                    table.insert(hidden_part, {{M2,C,M1,R,sens,u,"ca"},ld.Hiddenlinestyle,color,width,opacity})
                end
                if hidden  and (style ~= "noline") then
                    table.insert(hidden_part, {{A,C,M2,R,sens,u,"ca"},ld.Hiddenlinestyle,color,width,opacity,-arrowA})
                    table.insert(hidden_part, {{M1,C,B,R,sens,u,"ca"},ld.Hiddenlinestyle,color,width,opacity,arrowB})
                else
                    --self:Beginadvanced()
                    table.insert(before_sphere, {{A,C,M2,R,sens,u,"ca"},style,color,width,opacity,-arrowA})
                    table.insert(before_sphere, {{M1,C,B,R,sens,u,"ca"},style,color,width,opacity,arrowB})
                    --self:Endadvanced()
                end
            end
        else
            -- un des points est visible, l'autre non
            if visibledot(A) then -- A est visible, B non
                if sens == 1 then
                   table.insert(after_sphere, {{A,C,M2,R,sens,u,"ca"},style,color,width,opacity,-arrowA}) 
                   if hidden and hiddendelayed and (style ~= "noline") then
                        table.insert(hidden_part, {{A,C,M2,R,sens,u,"ca"},ld.Hiddenlinestyle,color,width,opacity})
                    end
                   if hidden  and (style ~= "noline") then 
                        table.insert(hidden_part, {{M2,C,B,R,sens,u,"ca"},ld.Hiddenlinestyle,color,width,opacity,arrowB})
                    else
                        --self:Beginadvanced()
                        table.insert(before_sphere, {{M2,C,B,R,sens,u,"ca"},style,color,width,opacity,arrowB})
                        --self:Endadvanced()
                    end
                else
                    table.insert(after_sphere, {{A,C,M1,R,sens,u,"ca"},style,color,width,opacity,-arrowA}) 
                    if hidden and hiddendelayed and (style ~= "noline") then
                        table.insert(hidden_part, {{A,C,M1,R,sens,u,"ca"},ld.Hiddenlinestyle,color,width,opacity})
                    end
                    if hidden  and (style ~= "noline") then 
                        table.insert(hidden_part, {{M1,C,B,R,sens,u,"ca"},ld.Hiddenlinestyle,color,width,opacity,arrowB})
                    else
                        --self:Beginadvanced()
                        table.insert(before_sphere, {{M1,C,B,R,sens,u,"ca"},style,color,width,opacity,arrowB})
                        --self:Endadvanced()
                    end
                end
            else -- B est visible, A non
                if sens == 1 then
                   table.insert(after_sphere, {{M1,C,B,R,sens,u,"ca"},style,color,width,opacity,arrowB}) 
                   if hidden and hiddendelayed and (style ~= "noline") then
                        table.insert(hidden_part, {{M1,C,B,R,sens,u,"ca"},ld.Hiddenlinestyle,color,width,opacity})
                    end
                   if hidden  and (style ~= "noline") then 
                        table.insert(hidden_part, {{A,C,M1,R,sens,u,"ca"},ld.Hiddenlinestyle,color,width,opacity,-arrowA})
                    else
                        --self:Beginadvanced()
                        table.insert(before_sphere, {{A,C,M1,R,sens,u,"ca"},style,color,width,opacity,-arrowA})
                        --self:Endadvanced()
                    end
                else
                    table.insert(after_sphere, {{M2,C,B,R,sens,u,"ca"},style,color,width,opacity,arrowB}) 
                    if hidden and hiddendelayed and (style ~= "noline") then
                        table.insert(hidden_part, {{M2,C,B,R,sens,u,"ca"},ld.Hiddenlinestyle,color,width,opacity})
                    end
                    if hidden  and (style ~= "noline") then 
                        table.insert(hidden_part, {{A,C,M2,R,sens,u,"ca"},ld.Hiddenlinestyle,color,width,opacity,-arrowA})
                    else
                        --self:Beginadvanced()
                        table.insert(before_sphere, {{A,C,M2,R,sens,u,"ca"},style,color,width,opacity,-arrowA})
                        --self:Endadvanced()
                    end
                end
            end
        end
    end
end

-- points sur la sphère en forme d'étoiles
function graph3d:DSstars(dots,options)
-- dots = liste de points sur la sphere
    if dots == nil then return end
    if pt3d.isPoint3d(dots) then dots = {dots} end
    options = options or {}
    local Ct, R = sphere.C, sphere.R
    local color = options.color or self.param.linecolor
    local scale = options.scale or 1
    local circled = options.circled or false
    local fill = options.fill or ""
    local width = options.width or self.param.linewidth
    local hidden = options.hidden
    if hidden == nil then hidden = ld.Hiddenlines end
    local long = 0.1*scale
    local dphi = long/R*ld.rad
    --if (dots == nil) then return end
    if pt3d.isPoint3d(dots) then dots = {dots} end
    dots = ld.map(ld.toSphere,dots)
    
    local drawAcross = function(A)
        local n1 = pt3d.prod(A-Ct,vecK)
        if pt3d.N1(n1) < 1e-10 then n1 = pt3d.prod(A-Ct,vecI) end
        n1 = pt3d.normalize(n1)
        local B1, C1 = ld.rotate3d(A,dphi,{Ct,n1}), ld.rotate3d(A,-dphi,{Ct,n1})
        local B2, C2 = ld.rotate3d(B1,60,{Ct,A-Ct}), ld.rotate3d(C1,60,{Ct,A-Ct})
        local B3, C3 = ld.rotate3d(B1,120,{Ct,A-Ct}), ld.rotate3d(C1,120,{Ct,A-Ct})
        if visibledot(A) then -- A est visible
            if fill ~= "" then
                self:DSfacet({B1,B2,B3,C1,C2,C3},{style="noline",fill=fill, width=1, fillopacity=0.5, hidden=hidden})
                self:DScircle(ld.plane(B1,C1,B2),{color=color, width=1, hidden=hidden}) 
                --local ct,r,n = ld.interPS(ld.plane(B1,C1,B2), {Ct,R})
                --local u = pt3d.normalize( pt3d.prod(n, pt3d.vecI) )
                --if u == nil then pt3d.normalize( pt3d.prod(n, pt3d.vecJ) ) end
                --table.insert(after_sphere, {{ct+r*u,ct,n,"c"},"solid",fill,1,0.5,0,fill,0.5}) 
            else
                self:DSarc({B1,C1},1,{color=color, width=width, hidden=hidden})
                self:DSarc({B2,C2},1,{color=color, width=width, hidden=hidden})
                self:DSarc({B3,C3},1,{color=color, width=width, hidden=hidden})
                if circled then self:DScircle(ld.plane(B1,C1,B2),{color=color, width=1, hidden=hidden}) end
            end
        else
            local old_hiddenlinestyle = ld.Hiddenlinestyle
            ld.Hiddenlinestyle = "solid"
            if fill ~= "" then
                --self:DSfacet({B1,B2,B3,C1,C2,C3},{style="noline",fill=insidelabelcolor, width=1, fillopacity=0.5, hidden=hidden})
                self:DScircle(ld.plane(B1,C1,B2),{color=insidelabelcolor, width=width, hidden=hidden})
            else
                --print(B1,C1,B2,C2,B3,C3)
                self:DSarc({B1,C1},1,{color=insidelabelcolor, width=width, hidden=hidden})
                self:DSarc({B2,C2},1,{color=insidelabelcolor, width=width, hidden=hidden})
                self:DSarc({B3,C3},1,{color=insidelabelcolor, width=width, hidden=hidden})
                if circled then self:DScircle(ld.plane(B1,C1,B2),{color=insidelabelcolor, width=1, hidden=hidden}) end
            end
            ld.Hiddenlinestyle = old_hiddenlinestyle
        end
    end
    for _, A in ipairs(dots) do
        drawAcross(A)
    end
end


-- facette sphérique (dessinée sur la sphère)
function graph3d:DSfacet(facet, options)
    facet = ld.map(ld.toSphere, facet)
    options = options or {}
    local style = options.style or self.param.linestyle
    local color = options.color or self.param.linecolor
    local width = options.width or self.param.linewidth
    local opacity = options.opacity or self.param.lineopacity    
    local hidden = options.hidden
    if hidden == nil then hidden = ld.Hiddenlines end
    local fillopacity = options.fillopacity or 0.3
    local fill = options.fill or ""
    local Ct, R = sphere.C, sphere.R
    local I,r,n = table.unpack(sphere.horizon)
    local P = {I, n}
    local N = self.Normal
    local cam = ld.camera
    if ld.projection_mode == "central" then N = ld.camera-Ct end
    local mat = self.matrix3d
    if not ld.isID3d(mat) then mat = ld.invmatrix3d(mat); N = ld.mLtransform3d(N,mat); cam = ld.mtransform3d(ld.camera,mat) end
    local chemV, chemH,M1,M2 = {}, {}
    local visiblelast, pred, M1, M2 = false
    facet = table.copy(facet)
    table.insert(facet,facet[1])
    for _,A in ipairs(facet) do
        if pt3d.dot(A-I,N) >=0 then --visibledot(A) then
            if visiblelast or (pred == nil) then 
                table.insert(chemV,{A,false})
            else -- pred is not visible
                local u =  pt3d.prod(pred-Ct,A-Ct)
                if pt3d.N1(u) < 1e-12 then u = vecK end
                if ld.projection_mode == "central" then
                    M2, M1 = ld.interCS({Ct,R,u}, {(Ct+cam)/2, pt3d.abs(Ct-cam)/2} )
                else
                    local n1 = pt3d.normalize( pt3d.prod(u,N) )
                    M1, M2 = Ct+R*n1, Ct-R*n1
                end
                if pt3d.det(pred-Ct,M1-Ct,u) < 0 then M1 = M2 end
                table.insert(chemH, {M1,true})                
                table.insert(chemV, {M1,true})
                table.insert(chemV, {A,false})
            end
            visiblelast = true
        else -- A not visible
            if (not visiblelast) or (pred == nil) then 
                table.insert(chemH,{A,false})
            else -- pred is visible
                local u =  pt3d.prod(pred-Ct,A-Ct)
                if pt3d.N1(u) < 1e-12 then u = vecK end
                if ld.projection_mode == "central" then
                    M2, M1 = ld.interCS({Ct,R,u}, {(Ct+cam)/2, pt3d.abs(Ct-cam)/2} )
                else
                    local n1 = pt3d.normalize( pt3d.prod(u,N) )
                    M1, M2 = Ct+R*n1, Ct-R*n1
                end
                if pt3d.det(pred-Ct,M1-Ct,u) < 0 then M1 = M2 end
                
                table.insert(chemV, {M1,true})
                table.insert(chemH, {M1,true})
                table.insert(chemH, {A,false})
            end
            visiblelast = false
        end
        pred = A
    end
   local V, U
    if #chemV > 0 then
        if chemV[1][1] ~= chemV[#chemV][1] then table.insert(chemV, chemV[1]) end
        V, U = chemV[1]
        local chem = {V[1]}
        for k = 2, #chemV do
            U = V; V = chemV[k]
            if pt3d.abs(U[1]-V[1]) > 1e-8 then
                if U[2] and V[2] then 
                    ld.insert(chem,{I,V[1],r,1,"ca"}) 
                else ld.insert(chem,{Ct,V[1],R,1,"ca"});
                end
            end
        end
        table.insert(after_sphere, {chem,style,color,width,opacity,0,fill,fillopacity})
        if hidden and hiddendelayed and (style ~= "noline") then
            table.insert(hidden_part, {chem,ld.Hiddenlinestyle,color,width,opacity})
        end
    end
    if #chemH > 0 then
        if chemH[1][1] ~= chemH[#chemH][1] then table.insert(chemH, chemH[1]) end
        V, U = chemH[1]
        chem = {V[1]}
        for k = 2, #chemH do
            U = V; V = chemH[k]
            if pt3d.abs(U[1]-V[1]) > 1e-8 then
                if U[2] and V[2] then ld.insert(chem,{I,V[1],r,1,"ca"}) else ld.insert(chem,{Ct,V[1],R,1,"ca"}) end
            end
        end    
        if hidden  and (style ~= "noline") then
            table.insert(hidden_part, {chem,ld.Hiddenlinestyle,color,width,opacity})
        else
            --self:Beginadvanced()
            table.insert(before_sphere, {chem,style,color,width,opacity,0,fill,fillopacity})
            --self:Endadvanced()
        end
    end
end


-- angle sphérique
function graph3d:DSangle(B,A,C,r,sens,options)
    A = ld.toSphere(A); B = ld.toSphere(B); C = ld.toSphere(C); 
    local C, R = sphere.C, sphere.R
    local alpha = r/R*ld.rad
    local C1 = ld.rotate3d(A, alpha, {Ct,pt3d.prod(A-Ct,C-Ct)})
    local B1 = ld.rotate3d(A, alpha, {Ct,pt3d.prod(A-Ct,B-Ct)})
    self:DSarc({B1,C1},sens,options)
end

-- labels
function graph3d:DSlabel(...)
-- options :
-- pos est "center" ou "N", "NE", "NE", "SE", "S", "SW", "W" ou "NW"
-- dist est la distance en cm du texte par rapport au node, si dist = nil c'est 0 par défaut
-- dir={dirX,dirY,dep} est la direction de l'écriture (nil pour le sens par défaut)
-- node_options est une chaîne passée directement à tikz, ex: "rotate=45, draw, fill=red" (options locales)
    local C, R = sphere.C, sphere.R
    local N = self.Normal
    local text, anchor = "", Origin
    local pos,dir,dist,node_options = self.param.labelstyle, nil, 0, ""
    
    local add_alabel = function()
        local u = anchor-C
        local options = {}
        options.pos = pos; options.dir = dir; options.dist = dist; options.node_options = node_options
        if pt3d.abs(u) < R then --anchor est dans la sphère
            if ld.Hiddenlines then
                local oldoptions = options.node_options
                local sep = ""
                if oldoptions ~= "" then sep = "," end
                options.node_options = oldoptions..sep..insidelabelcolor
                table.insert(hidden_part, {text,anchor,table.copy(options)})
                options.node_options = oldoptions
            else
                --self:Beginadvanced()
                table.insert(before_sphere, {text,anchor,options})
                --self:Endadvanced()
            end
        else -- anchor est à l'extérieur de la sphère
            if visibledot(anchor) then -- anchor est visible
                table.insert(after_sphere, {text,anchor,options})
            else
                --self:Beginadvanced()
                table.insert(before_sphere, {text,anchor,options})
                --self:Endadvanced()
            end
        end
    end
    for k, x in ipairs{...} do
        if k%3 == 1 then text = x
        elseif k%3 == 2 then anchor = x
        else -- options
            pos = x.pos or pos
            dir = x.dir or dir
            dist = x.dist or dist
            node_options = x.node_options or node_options
            add_alabel()
        end
    end
end

-- points dans la scène
function graph3d:DSdots(dots,options)
    if dots == nil then return end
    if pt3d.isPoint3d(dots) then dots = {dots} end
    options = options or {}
    local C, R = sphere.C, sphere.R
    local N = self.Normal
    local sep
    local hidden = options.hidden
    if hidden == nil then hidden = ld.Hiddenlines end
    local mark_options = options.mark_options or ""
    if mark_options == "" then sep = "" else sep = "," end
    local add_adot = function(A)
        local u = A-C
        local d = pt3d.abs(u)
        if (d < R) or ((math.abs(d-R)<1e-8) and (not visibledot(A))) then --A est dans la sphère ou caché sur la sphère
            if hidden then
                table.insert(hidden_part, {A,mark_options..sep..insidelabelcolor})
            else
                --self:Beginadvanced()
                table.insert(before_sphere, {A,mark_options})
                --self:Endadvanced()
            end
        else -- A est à l'extérieur de la sphère ou dessus mais visible
            if visibledot(A) then -- A est visible
                table.insert(after_sphere, {A,mark_options})
            else
                --self:Beginadvanced()
                table.insert(before_sphere, {A,mark_options})
                --self:Endadvanced()
            end
        end
    end
    for _, A in ipairs(dots) do
            add_adot(A)
    end
end

-- courbe spherique
function graph3d:DScurve(L,options)
-- L est une ligne polygonale 3d représentant une courbe tracée sur la sphère courante
    if (L == nil) or (type(L) ~= "table") then return end
    options = options or {}
    local style = options.style or self.param.linestyle
    local color = options.color or self.param.linecolor
    local width = options.width or self.param.linewidth
    local opacity = options.opacity or self.param.lineopacity
    local hidden = options.hidden
    if hidden == nil then hidden = ld.Hiddenlines end
    local out = options.out -- ends of hidden parts
    local C, R = sphere.C, sphere.R
    local N = self.Normal
    local Visible, Hidden = {},{}
    local visible, hidden, etat, Avisible = {}, {}
    local visible_function = function(A)
        return visibledot(A)
    end
    Visible, Hidden =  ld.split_points_by_visibility(L,visible_function)
    if out ~= nil then
        for _,F in ipairs(Hidden) do
            table.insert(out,F[1])
            if F[#F] ~= F[1] then table.insert(out,F[#F]) end
        end
    end
    local rep = {}
    for _, visible in ipairs(Visible) do
        if #visible > 1 then
            table.insert(visible,2,"m"); table.insert(visible,"l")
            ld.insert(rep,visible)
        end
    end
    table.insert(after_sphere, {rep,style,color,width,opacity})
    if hidden and hiddendelayed and (style ~= "noline") then
        table.insert(hidden_part, {rep,ld.Hiddenlinestyle,color,width,opacity})
    end
    rep = {}
    for _, hidden in ipairs(Hidden) do
        if #hidden > 1 then
            table.insert(hidden,2,"m"); table.insert(hidden,"l")
            ld.insert(rep,hidden)
        end
    end
    if hidden  and (style ~= "noline") then
        table.insert(hidden_part, {rep,ld.Hiddenlinestyle,color,width,opacity})
    else
       table.insert(before_sphere, {rep,style,color,width,opacity})
    end
end

-- plan
function graph3d:DSplane(P,args) -- draw a plane around the sphere
    args = args or {}
    local scale = args.scale or 1
    local angle = args.angle or 0
    local trace = args.trace -- to show or not the circle on the sphere
    if trace == nil then trace = true end
    local R = sphere.R
    local A, n = table.unpack(P)
    A = ld.proj3d(Origin,P)
    n = pt3d.normalize(n)
    if pt3d.dot(n,vecK) < 0 then n = -n end
    local F = {M(R+1,-R-1,0),M(R+1,R+1,0),M(-R-1,R+1,0),M(-R-1,-R-1,0)}
    F = ld.shift3d( ld.sym3d( ld.scale3d(F,scale),{Origin,vecK+n}), A)
    if angle ~= 0 then F = ld.rotate3d(F,angle,{A,n}) end
    args.close = true
    self:DSpolyline(F,args)
    if trace then self:DScircle(P,args) end
end


function graph3d:DSinvstereo_curve(L,options)
-- L est une ligne polygonale 3d représentant une courbe tracée sur plan d'équation z = cte
-- cette courbe est dessinée sur la sphère par stéréographie inverse
    if (L == nil) or (type(L) ~= "table") then return end
    local C, R = sphere.C, sphere.R
    local N = C+R*vecK
    self:DScurve(ld.inv_projstereo(L,{C,R},N), options)
end

function graph3d:DSinvstereo_polyline(L,options)
-- L est une ligne polygonale 3d tracée sur un plan d'équation z = cte
-- cette ligne est dessinée sur la sphère par stéréographie inversée (chaque segment devient un arc de cercle sur la sphère)
    if (L == nil) or (type(L) ~= "table") then return end
    options = options or {}
    local close = options.close or false   
    if pt3d.isPoint3d(L[1]) then L = {L} end
    for _,cp in ipairs(L) do
        local f,len = ld.curvilinear_param3d(cp,close)
        local n = math.max(math.floor(5*len),25)
        local L1 = ld.parametric3d(f,0,1,n,false,1)[1]
        self:DSinvstereo_curve(L1,options)
    end
end
