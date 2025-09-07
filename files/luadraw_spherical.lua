-- luadraw_spherical.lua 
-- date 2025/09/07
-- version 2.1
-- Copyright 2025 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.


-- to draw circle on a sphere, big circle, arc, facet, segment, polyline, label and spherical curve
-- provides:
    -- Define_sphere( args ), args = list of fields (with default values)
        -- args={center= (Origin), radius= (3),color= (orange),opacity= (1) (1),mode= (2),edgecolor= (gray),edgestyle= (solid),edgewidth= (4),hinddencolor= (gray),hiddentstyle=Hiddenlinestyle,show= (true)}
    -- sM(x,y,z) : returns point on sphere
    -- sM(theta,phi) : returns point on sphere
    -- toSphere(A): returns projection of A on sphere
    
-- drawing methods:
    -- g:DScircle(P, options) : P={A,n} is a plane, options = {style=, color=, width=, opacity=, out = varaible}
    
    -- g:DSbigcircle(AB,options) -- AB = {A,B} (two points of sphere)
    
    -- g:DSseg(seg,options) -- seg={A,B} (segment), options = {style=, color=, width=, opacity=, arrows=}
    
    -- g:DSline(line,options) -- seg={A,B} (segment), options = {style=, color=, width=, opacity=, arrows=, scale=}
    
    -- g:DSpolyline(L,options) -- L = 3d polyline, options = {style=, color=, width=, opacity=, arrows=}
    
    -- g:DSarc(AB,sens,options)  -- arc of big circle from A to B on sphere, options = {style=, color=, width=, opacity=, arrows=, normal=}
    
    -- gr:DSfacet(facet, options) facet = list of points on sphere, options = {style=, color=, width=, opacity=, fill=, fillopacity=}
    
    -- g:DSangle(B,A,C,r,sens,options) -- A, B, C three points on sphere, r=radius, options = {style=, color=, width=, opacity=, arrows=} (calls DSarc)
    
    -- g:DSlabel(text,anchor,options, text, anchor, options,...) (the color of label inside sphere is Insidelabelcolor (DarkGray by default))
    
    --g:DScurve(L,options) L is a 3d polyline representing a curve on sphere, options = {style=, color=, width=, opacity=}
    
    -- Dsplane(P,options) draw a plane around the sphere, P={A,n} is a plane, options = {style=, color=, width=, opacity=, scale= (1), angle= (0), trace= (true)}

--last instruction for showing the scene:
    -- g:DSspherical() with no argument.
    

Insidelabelcolor = "DarkGray"
arrowBstyle = "->"
arrowAstyle = "<-"
arrowABstyle = "<->"

local sphere = {["C"]=Origin, ["R"]=3, 
    ["color"]="orange",
    ["opacity"]=1,
    ["mode"]=2, 
    ["edgecolor"]="LightGray", 
    ["edgestyle"] = "solid",
    ["edgewidth"] = "4",
    ["hiddenstyle"] = Hiddenlinestyle,
    ["hiddencolor"] = "gray",
    ["show"] = true
    } -- sphere definition
    
local before_sphere = {}
local after_sphere = {}
local hidden_part = {}

function clear_spherical()
    before_sphere = {}
    after_sphere = {}
    hidden_part = {}
    sphere = {["C"]=Origin, ["R"]=3, 
    ["color"]="orange",
    ["opacity"]=1,
    ["mode"]=2, 
    ["edgecolor"]="LightGray", 
    ["edgestyle"] = "solid",
    ["edgewidth"] = "4",
    ["hiddenstyle"] = Hiddenlinestyle,
    ["hiddencolor"] = "gray",
    ["show"] = true
    } -- sphere definition
    Insidelabelcolor = "DarkGray"
    arrowBstyle = "->"
    arrowAstyle = "<-"
    arrowABstyle = "<->"
end

function graph3d:Define_sphere( args )
    args = args or {}
    if args.center ~= nil then sphere.C = args.center end
    if args.radius ~= nil then sphere.R = args.radius end
    if args.color ~= nil then  sphere.color = args.color end
    if args.opacity ~= nil then sphere.opacity = args.opacity end
    if args.mode ~= nil then sphere.mode = args.mode end
    if args.edgecolor ~= nil then sphere.edgecolor = args.edgecolor end
    if args.edgestyle ~= nil then sphere.edgestyle = args.edgestyle end
    if args.hiddenstyle ~= nil then sphere.hiddenstyle = args.hiddenstyle end
    if args.hiddencolor ~= nil then sphere.hiddencolor = args.hiddencolor end
    if args.edgewidth ~= nil then sphere.edgewidth = args.edgewidth end
    if args.show ~= nil then sphere.show = args.show end
end

function sM(x,y,z) -- or sM(theta,phi) (theta, phi degrees), define a spherical dot in (C,vecI,vecJ,vecK)
    local C, R = sphere.C, sphere.R
    if z ~= nil then --cartesian coordinates
        return C + R*pt3d.normalize(M(x,y,z))
    else -- spherical coordinates
        return C + Ms(R,x*deg,y*deg)
    end
end

function toSphere(A)
    local C, R = sphere.C, sphere.R
    local u = A-C
    if pt3d.N1(u)< 1e-12 then return end
    return C+R*pt3d.normalize(u)
end

function graph3d:Dspherical()

    local display_elt = function(elt) 
    -- elt={path,style,color,width,opacity,arrows} ou
    -- elt={text,anchor,options} (labels)
        if type(elt[1]) == "string" then -- label
            self:Dlabel3d(elt[1],elt[2],elt[3])
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
    local oldlinestyle = self.param.linestyle
    local oldlinecolor = self.param.linecolor
    local oldlinewidth = self.param.linewidth
    local oldlineopacity = self.param.lineopacity
    local oldfillstyle = self.param.fillstyle
    local oldfillcolor = self.param.fillcolor
    local oldfillopacity = self.param.fillopacity
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
   for _, elt in ipairs(hidden_part) do
        display_elt(elt)
    end    
    self:Lineoptions(oldlinestyle, oldlinecolor, oldlinewidth); self:Lineopacity(oldlineopacity)
    self:Filloptions(oldfillstyle,oldfillcolor,oldfillopacity)
    clear_spherical()
end

-- ajouter un cercle tracé sur la sphère
function graph3d:DScircle(P,options) -- P={A,u} (plane)
-- options = {style=, color=, width=, opacity=, out=}
    options = options or {}
    local style = options.style or self.param.linestyle
    local color = options.color or self.param.linecolor
    local width = options.width or self.param.linewidth
    local opacity = options.opacity or self.param.lineopacity
    local out = options.out -- returns end points of hidden part if it exists
    
    local C, R = sphere.C, sphere.R
    local N, angle = self.Normal
    
    local acircle = function(I,r,v,u) -- when we have to draw a circle
        local w = pt3d.prod(u,vecI)
        if pt3d.N1(w) < 1e-12 then w =pt3d.prod(u,vecJ) end
        local J = I+r*pt3d.normalize(w) -- a point of the circle
        if (angle == 0) or (pt3d.dot(v,N) > 0) then -- visible
            table.insert(after_sphere, {{J,I,u,"c"},style,color,width,opacity})
        elseif Hiddenlines and (style ~= "noline") then
            table.insert(hidden_part, {{J,I,u,"c"},Hiddenlinestyle,color,width,opacity})
        else 
            --self:Beginadvanced()
            table.insert(before_sphere, {{J,I,u,"c"},style,color,width,opacity})
            --self:Endadvanced()
        end
    end
    local A, b, c, u
    if #P == 3 then
        A, b, c = table.unpack(P)
        A= toSphere(A); b = toSphere(b); c = toSphere(c)
        u = pt3d.prod(b-A,c-A)
        P = {A,u}
    else
        A, u = table.unpack(P)
    end
    local I = proj3d(C,P) -- center 
    if pt3d.dot(u,I-C) < 0 then u = -u end
    local d = pt3d.abs(C-I)
    if d >= R then  return end -- no circle
    local v, r = I-C
    if pt3d.N1(v) < 1e-12 then -- C is on P (big circle)
        v = u; r = R; angle = 0
    else
        r = math.sqrt(R^2- pt3d.abs2(v))
    end
    if pt3d.N1(pt3d.prod(u,N))< 1e-12 then -- P has the same direction than screen 
        acircle(I,r,v,u,angle)
    else
        local n2 = pt3d.normalize(pt3d.prod(N,v))
        local n1 = pt3d.normalize(pt3d.prod(v,n2))
        if angle == nil then angle = -pt3d.dot(v,N)/(r*pt3d.dot(n1,N)) end
        if math.abs(angle) > 1+1e-6 then acircle(I,r,v,u,angle)
        elseif (1<angle) and (angle<1+1e-6) then angle = 1 
        elseif (angle <-1) and (1-1e-6<angle) then angle = -1 
        else
            local t0 = math.acos(angle)
            if math.abs(t0) < 1e-6 then acircle(I,r,v,u)
            else
                local A, B = I+r*math.cos(t0)*n1+r*math.sin(t0)*n2, I+r*math.cos(t0)*n1-r*math.sin(t0)*n2
                if out ~= nil then
                    insert(out,{A,B})
                end
                local sens = 1
                if pt3d.dot(I-C+r*n1,N) < 0 then 
                    sens = -1
                end
                table.insert(after_sphere, {{A,I,B,r,sens,u,"ca"},style,color,width,opacity})
                if Hiddenlines  and (style ~= "noline") then 
                    table.insert(hidden_part, {{A,I,B,r,-sens,u,"ca"},Hiddenlinestyle,color,width,opacity})
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
    A = toSphere(A); B = toSphere(B)
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
    local arrows = options.arrows or 0 --0/1/2
    local out = options.out -- intersections points between seg and sphere
    local arrowA, arrowB = 0, 0
    if arrows == 1 then arrowB = 1
    elseif arrows == 2 then arrowA = 1; arrowB = 1
    end
    
    local add_seg_out = function(U,V,arrow)
        local dev, der = splitseg({U,V},{C,self.Normal})
        if #der ~= 0 then 
            table.insert(der,"l"); 
            --self:Beginadvanced()
            table.insert(before_sphere, {der,style,color,width,opacity,arrow} )
            --print(style)
            --self:Endadvanced()
        end
        if #dev ~= 0 then 
            table.insert(dev,"l")
            table.insert(after_sphere, {dev,style,color,width,opacity,arrow}) 
        end
    end

    if Hiddenlines  and (style ~= "noline") then 
        table.insert(hidden_part, {{A,B,"l"},Hiddenlinestyle,color,width,opacity,arrows})  -- whole seg
    else -- pas de lignes cachées
        --self:Beginadvanced()
        table.insert(before_sphere, {{A,B,"l"},style,color,width,opacity,arrows})
        --self:Endadvanced()
    end
    local I = dproj3d(C,{A,B-A})
    if pt3d.abs(C-I) >= R then 
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
        elseif (t1 > 0) and (t1<=ell) and (t2>=ell) then
            add_seg_out(J,A,arrowA) -- [A,J] is out S, [J,B] is in S
            if out ~= nil then table.insert(out,J) end
        elseif (t1 > 0)  and (t2 < ell) then
            add_seg_out(J,A,arrowA) -- [A,J] is out S, -- [J,K] is in S
            add_seg_out(K,B,arrowB) -- [K,B] is out S
            if out ~= nil then insert(out,{J,K}) end
        elseif (t1 <= 0) and (t2 < ell) then 
            add_seg_out(K,B,arrowB) -- [A,K] is in S, [K,B] is out S
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
    options.arrows = options.arrows or 0 --0/1/2
    local arrows = options.arrows
    local close = options.close or false
    if isPoint3d(L[1]) then L = {L} end
    local oldstyle = Hiddenlinestyle
    Hiddenlinestyle = "noline"
    for _, cp in ipairs(L) do
        local B, A = cp[1] 
        if close then table.insert(cp,B) end
        if Hiddenlines  and (options.style ~= "noline") then 
            table.insert(hidden_part, {concat(cp,{"l"}),oldstyle,options.color,options.width,options.opacity,options.arrows})  -- whole polyline
        end
        local n = #cp
        for k = 2, n do
            A = B; B = cp[k]
            options.arrows = 0
            if (arrows == 2) then
                if (k == 2) and (k == n) then options.arrows = 2
                elseif (k == 2) then options.arrows = -1
                elseif (k == n) then options.arrows = 1
                end 
            elseif (arrows == 1) then
                if (k == n) then options.arrows = 1 end
            end
            self:DSseg({A,B},options)
        end
    end
    Hiddenlinestyle = oldstyle
end

-- ajouter un arc de grand cercle sur la sphère
function graph3d:DSarc(AB,sens,options)
-- options = {style=, color=, width=, opacity=, arrows=, normal=}
    options = options or {}
    local A, B = table.unpack(AB)
    local style = options.style or self.param.linestyle
    local color = options.color or self.param.linecolor
    local width = options.width or self.param.linewidth
    local opacity = options.opacity or self.param.lineopacity
    local arrows = options.arrows or 0 --0/1/2
    local normal = options.normal or vecK
    local arrowA, arrowB = 0, 0
    if arrows == 1 then arrowB = 1
    elseif arrows == 2 then arrowA = 1; arrowB = 1
    end
    local C, R = sphere.C, sphere.R
    local N = self.Normal
    local A, B = toSphere(A), toSphere(B) -- to have points on sphere
    local u = pt3d.prod(A-C,B-C)
    if pt3d.N1(u) < 1e-12 then 
        if normal ~= nil then 
            u = normal 
        else
            u = pt3d.prod(B-A,vecI)
            if pt3d.N1(u) < 1e-12 then u = pt3d.prod(B-A,vecJ) end
        end
    end -- points alignés avec le centre !
    if pt3d.N1(pt3d.prod(u,N))< 1e-12 then -- P est le plan de l'écran
        table.insert(after_sphere, {{A,C,B,R,sens,n,"ca"},style,color,width,opacity,arrows} )
    else
        local n2 = pt3d.normalize(pt3d.prod(N,u))
        local n1 = pt3d.normalize(pt3d.prod(n2,u))
        local M1, M2, M = C+R*n2, C-R*n2
        local xa = pt3d.dot(A-C,n1) -- abscisse de A dans (C,n1,n2)
        if (pt3d.dot(A-C,N) >= 0) and (pt3d.dot(B-C,N) >= 0) then -- A et B sont visibles
            if sens == 1 then
                table.insert(after_sphere, {{A,C,B,R,sens,u,"ca"},style,color,width,opacity,arrowB})
            else
                table.insert(after_sphere, {{A,C,M1,R,sens,u,"ca"},style,color,width,opacity,-arrowA})
                table.insert(after_sphere, {{M2,C,B,R,sens,u,"ca"},style,color,width,opacity,arrowB})
                if Hiddenlines  and (style ~= "noline") then
                    table.insert(hidden_part, {{M1,C,M2,R,sens,u,"ca"},Hiddenlinestyle,color,width,opacity,0})
                else
                    --self:Beginadvanced()
                    table.insert(before_sphere, {{M1,C,M2,R,sens,u,"ca"},style,color,width,opacity,0})
                    --self:Endadvanced()
                end
            end
        elseif (pt3d.dot(A-C,N) < 0) and (pt3d.dot(B-C,N) < 0) then -- A et B sont cachés
            if sens == 1 then
                if Hiddenlines  and (style ~= "noline") then
                    table.insert(hidden_part, {{A,C,B,R,sens,u,"ca"},Hiddenlinestyle,color,width,opacity,-arrowA})
                else
                    --self:Beginadvanced()
                    table.insert(before_sphere, {{A,C,B,R,sens,u,"ca"},style,color,width,opacity,arrows})
                    --self:Endadvanced()
                end
            else
                table.insert(after_sphere, {{M2,C,M1,R,sens,u,"ca"},style,color,width,opacity,0})
                if Hiddenlines  and (style ~= "noline") then
                    table.insert(hidden_part, {{A,C,M2,R,sens,u,"ca"},Hiddenlinestyle,color,width,opacity,-arrowA})
                    table.insert(hidden_part, {{M1,C,B,R,sens,u,"ca"},Hiddenlinestyle,color,width,opacity,arrowB})
                else
                    --self:Beginadvanced()
                    table.insert(before_sphere, {{A,C,M2,R,sens,u,"ca"},style,color,width,opacity,-arrowA})
                    table.insert(before_sphere, {{M1,C,B,R,sens,u,"ca"},style,color,width,opacity,arrowB})
                    --self:Endadvanced()
                end
            end
        else
            -- un des points est visible, l'autre non
            if pt3d.dot(B-C,N) <= 0 then -- A est visible, B non
                if sens == 1 then
                   table.insert(after_sphere, {{A,C,M2,R,sens,u,"ca"},style,color,width,opacity,-arrowA}) 
                   if Hiddenlines  and (style ~= "noline") then 
                        table.insert(hidden_part, {{M2,C,B,R,sens,u,"ca"},Hiddenlinestyle,color,width,opacity,arrowB})
                    else
                        --self:Beginadvanced()
                        table.insert(before_sphere, {{M2,C,B,R,sens,u,"ca"},style,color,width,opacity,arrowB})
                        --self:Endadvanced()
                    end
                else
                    table.insert(after_sphere, {{A,C,M1,R,sens,u,"ca"},style,color,width,opacity,-arrowA}) 
                    if Hiddenlines  and (style ~= "noline") then 
                        table.insert(hidden_part, {{M1,C,B,R,sens,u,"ca"},Hiddenlinestyle,color,width,opacity,arrowB})
                    else
                        --self:Beginadvanced()
                        table.insert(before_sphere, {{M1,C,B,R,sens,u,"ca"},style,color,width,opacity,arrowB})
                        --self:Endadvanced()
                    end
                end
            else -- B est visible, A non
                if sens == 1 then
                   table.insert(after_sphere, {{M1,C,B,R,sens,u,"ca"},style,color,width,opacity,arrowB}) 
                   if Hiddenlines  and (style ~= "noline") then 
                        table.insert(hidden_part, {{A,C,M1,R,sens,u,"ca"},Hiddenlinestyle,color,width,opacity,-arrowA})
                    else
                        --self:Beginadvanced()
                        table.insert(before_sphere, {{A,C,M1,R,sens,u,"ca"},style,color,width,opacity,-arrowA})
                        --self:Endadvanced()
                    end
                else
                    table.insert(after_sphere, {{M2,C,B,R,sens,u,"ca"},style,color,width,opacity,arrowB}) 
                    if Hiddenlines  and (style ~= "noline") then 
                        table.insert(hidden_part, {{A,C,M2,R,sens,u,"ca"},Hiddenlinestyle,color,width,opacity,-arrowA})
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

-- facette sphérique (dessinée sur la sphère)
function graph3d:DSfacet(facet, options)
    facet = map(toSphere, facet)
    options = options or {}
    local style = options.style or self.param.linestyle
    local color = options.color or self.param.linecolor
    local width = options.width or self.param.linewidth
    local opacity = options.opacity or self.param.lineopacity    
    local fillopacity = options.fillopacity or 0.3
    local fill = options.fill or ""
    local Ct, R = sphere.C, sphere.R
    local dev, der = splitfacet(facet, {Ct, self.Normal})
    local chem, U, V = {}
    if dev ~= nil then
        V, U = dev[1]
        chem = {V}; table.insert(dev,V)
        for k = 2, #dev do
            U = V; V = dev[k]
            insert(chem,{Ct,V,R,1,"ca"})
        end
        table.insert(after_sphere, {chem,style,color,width,opacity,0,fill,fillopacity})
    end    
    if der ~= nil then
        V, U = der[1]
        chem = {V}; table.insert(der,V)
        for k = 2, #der do
            U = V; V = der[k]
            insert(chem,{Ct,V,R,1,"ca"})
        end
        if Hiddenlines  and (style ~= "noline") then
            table.insert(hidden_part, {chem,Hiddenlinestyle,color,width,opacity})
        else
            --self:Beginadvanced()
            table.insert(before_sphere, {chem,style,color,width,opacity,0,fill,fillopacity})
            --self:Endadvanced()
        end
    end
end

-- angle sphérique
function graph3d:DSangle(B,A,C,r,sens,options)
    A = toSphere(A); B = toSphere(B); C = toSphere(C); 
    local C, R = sphere.C, sphere.R
    local alpha = r/R*rad
    local C1 = rotate3d(A, alpha, {Ct,pt3d.prod(A-Ct,C-Ct)})
    local B1 = rotate3d(A, alpha, {Ct,pt3d.prod(A-Ct,B-Ct)})
    self:DSarc({B1,C1},sens,options)
end

-- labels
function graph3d:DSlabel(...)
    local C, R = sphere.C, sphere.R
    local N = self.Normal
    local text, anchor, options = "", Origin, {}
        
    local add_alabel = function()
        local u = anchor-C
        if pt3d.abs(u) < R then --anchor est dans la sphère
            if Hiddenlines then
                options.node_options=Insidelabelcolor; table.insert(hidden_part, {text,anchor,options})
            else
                --self:Beginadvanced()
                table.insert(before_sphere, {text,anchor,options})
                --self:Endadvanced()
            end
        else -- anchor est à l'extérieur de la sphère
            if pt3d.dot(N,u) >= 0 then -- anchor est visible
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
        else 
            options = x
            add_alabel()
        end
    end
end

-- courbe spherique
function graph3d:DScurve(L,options)
-- L est une ligne polygonale 3d représentant une courbe tracée sur la sphère courante
    options = options or {}
    local style = options.style or self.param.linestyle
    local color = options.color or self.param.linecolor
    local width = options.width or self.param.linewidth
    local opacity = options.opacity or self.param.lineopacity
    local out = options.out -- ends of hidden parts
    local C, R = sphere.C, sphere.R
    local N = self.Normal
    local Visible, Hidden = {},{}
    local visible, hidden, etat, Avisible = {}, {}
    if L == nil then return end
    if isPoint3d(L[1]) then L = {L} end
    for _, cp in ipairs(L) do
        visible, hidden = {}, {}
        etat = 0
        for _, A in ipairs(cp) do
            Avisible = (pt3d.dot(A-C,N) >= 0)
            if Avisible then -- A is visible
                table.insert(visible,A)
                if etat == 2 then-- previous dot was not visible
                    table.insert(Hidden, hidden); hidden = {}
                end
                etat = 1
            else -- A not visible
                table.insert(hidden,A)
                if etat == 1 then -- previous dot was visible
                    table.insert(Visible, visible); visible = {}
                end
                etat = 2
            end
        end
        if #visible > 1 then table.insert(Visible, visible) end
        if #hidden > 1 then  table.insert(Hidden, hidden) end
    end
    Visible = merge3d(Visible); Hidden = merge3d(Hidden)
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
            insert(rep,visible)
        end
    end
    table.insert(after_sphere, {rep,style,color,width,opacity})
    rep = {}
    for _, hidden in ipairs(Hidden) do
        if #hidden > 1 then
            table.insert(hidden,2,"m"); table.insert(hidden,"l")
            insert(rep,hidden)
        end
    end
    if Hiddenlines  and (style ~= "noline") then
        table.insert(hidden_part, {rep,Hiddenlinestyle,color,width,opacity})
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
    A = proj3d(Origin,P)
    n = pt3d.normalize(n)
    if pt3d.dot(n,vecK) < 0 then n = -n end
    local F = {M(R+1,-R-1,0),M(R+1,R+1,0),M(-R-1,R+1,0),M(-R-1,-R-1,0)}
    F = shift3d( sym3d( scale3d(F,scale),{Origin,vecK+n}), A)
    if angle ~= 0 then F = rotate3d(F,angle,{A,n}) end
    args.close = true
    self:DSpolyline(F,args)
    if trace then self:DScircle(P,args) end
end
