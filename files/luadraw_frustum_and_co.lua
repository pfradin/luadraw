-- luadraw_frustum_and_co.lua 
-- date 2026/09/05
-- version 3.5
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   https://www.ctan.org/license/lppl

-- functions for : cylinder, cone, frustum and sphere

local ld = luadraw
local pt3d = ld.pt3d

--------------------------- outlines -----------------------------------

function ld.graph3d:Cone_outline(B, R, V, H) -- or Cone_outline(B, R, H)
-- returns a table:
-- {side=<path3>, section=<list of paths>, visible=<path>, hidden=<path>, tangency=<list of dots>, angle=<angle in degrees>}
    if not pt3d.isPoint3d(H) then -- format B, R, H
        H = V; V = H-B
    end
    if pt3d.dot(V,H-B) < 0 then V = -V end
    local rep = {}
    local sin, cos, pi = math.sin, math.cos, math.pi
    local O, I, J = ld.orthoframe({B, V})
    local mat, H1, V1 = self.matrix3d, H, V
    if not ld.isID3d(mat) then
        O, I, J = ld.mtransform3d(O,mat), ld.mLtransform3d(I,mat), ld.mLtransform3d(J,mat)
        H1 = ld.mtransform3d(H,mat); V1 = ld.mLtransform3d(V,mat)
    end
    local angle = self:Arg(self:Proj3d(H)-self:Proj3d(B))*ld.rad
    if angle < 0 then angle = angle+180
        elseif angle > 180 then angle = angle-180 
    end 
    rep.visible = {}
    rep.hidden = {}
    rep.side = {}
    rep.section = {}
    rep.angle = angle
    local f = function(t)
        local  A = O + R*(cos(t)*I+sin(t)*J)
        local N = self.Normal
        if ld.projection_mode == "central" then N = ld.camera-A end
        return pt3d.det(N, A-H1, R*(-sin(t)*I+cos(t)*J) )
    end
    local T = ld.solve(f,-pi,pi)
    if (T ~= nil) and (#T >= 2) then
        local M1, M2 = O+R*(cos(T[1])*I+sin(T[1])*J), O+R*(cos(T[2])*I+sin(T[2])*J)
        local t = (T[1]+T[2])/2
        local M3 = O+R*(cos(t)*I+sin(t)*J)
        if not ld.isID3d(mat) then
            M1, M2 = table.unpack( ld.mtransform3d({M1, M2}, ld.invmatrix3d(mat)) )
        end
        rep.tangency = {M1,M2}
        local sens = 1
        if self:Cosine_incidence(M3-O,O) < 0 then sens = -1 end
        if self:Cosine_incidence(V1,O) < 0 then -- circular base visible
            rep.visible = {M1, "m", H, M2, "l", B, -V, "c" }
            table.insert(rep.section, {M1,B,V,"c"})
        else
            rep.visible = {M1, "m", B, M2, R, sens, V, "ca", H, "l", "cl" }
            rep.hidden = {M1, "m", B, M2, R, -sens, V, "ca"}
        end
        rep.side = {M1, "m", H, M2,"l", B, M1, R, -sens, V, "ca"}
    else
        O, I, J = ld.orthoframe({B, V})
        rep.visible = {B+R*I,B,V,"c"}
        if self:Cosine_incidence(V1,O) < 0 then -- circular base visible
            table.insert(rep.section, {B+R*I,B,V,"c"})
        else 
            rep.side = {B+R*I,B,V,"c"}
        end
    end
    return rep
end

function ld.graph3d:Cylinder_outline(B, R, V, H) -- or Cylinder_outline(B, R, H)
-- returns a table:
-- {side=<path3>, section=<list of paths>, visible=<path>, hidden=<path>, tangency=<list of dots>, angle=<angle in degrees>}
    if not pt3d.isPoint3d(H) then -- format B, R, H
        H = V; V = H-B
    end
    if pt3d.dot(V,H-B) < 0 then V = -V end
    local U = H-B
    local rep = {}
    local sin, cos, pi = math.sin, math.cos, math.pi
    local O, I, J = ld.orthoframe({B, V})
    local mat, H1, V1 = self.matrix3d, H, V
    if not ld.isID3d(mat) then
        O, I, J = ld.mtransform3d(O,mat), ld.mLtransform3d(I,mat), ld.mLtransform3d(J,mat)
        H1 = ld.mtransform3d(H,mat); V1 = ld.mLtransform3d(V,mat)
    end
    local angle = self:Arg(self:Proj3d(H)-self:Proj3d(B))*ld.rad
    if angle < 0 then angle = angle+180
        elseif angle > 180 then angle = angle-180 
    end
    local f = function(t)
        local  A = O + R*(cos(t)*I+sin(t)*J)
        local N = self.Normal
        if ld.projection_mode == "central" then N = ld.camera-A end
        return pt3d.det(N, H1-O, R*(-sin(t)*I+cos(t)*J) )
    end
    rep.visible = {}
    rep.hidden = {}
    rep.side = {}
    rep.section = {}
    rep.angle = angle
    local T = ld.solve(f,-pi,pi)
    if (T ~= nil) and (#T >= 2) then
        local M1, M2 = O+R*(cos(T[1])*I+sin(T[1])*J), O+R*(cos(T[2])*I+sin(T[2])*J)
        local t = (T[1]+T[2])/2
        local M3 = O+R*(cos(t)*I+sin(t)*J)
        if not ld.isID3d(mat) then
            M1, M2 = table.unpack( ld.mtransform3d({M1, M2}, ld.invmatrix3d(mat)) )
        end
        rep.tangency = {M1, M2, M1+U, M2+U}
        local sens = 1
        if self:Cosine_incidence(M3-O,O) < 0 then sens = -1 end
        rep.visible = {M1, "m", M1+U, "l"}
        if self:Cosine_incidence(V1,H1) > 0 then -- second circular base visible
            table.append(rep.visible, {H, V, "c", M2+U, "m"})
            table.insert(rep.section, {M1+U,H,V,"c"})
        else
            table.append(rep.visible, {H, M2+U, R, sens, V, "ca"})
            table.append(rep.hidden, {M1+U, "m", H, M2+U, R, -sens, V, "ca"})
        end
        table.append(rep.visible, {M2, "l"})
        if self:Cosine_incidence(V1,O) < 0 then -- first circular base visible        
            table.append(rep.visible, {B, V, "c"})
            table.insert(rep.section, {M1,B,V,"c"})
        else -- first circular base not visible
            table.append(rep.visible, {B, M1, R, -sens, V, "ca"} )
            table.append(rep.hidden, {M2, "m", B, M1, R, sens, V, "ca"} )
        end
        rep.side = {M1,"m",M1+U,"l",H,M2+U,R,sens,V,"ca",M2,"l",B,M1,R,-sens,V,"ca"}
    else
        O, I, J = ld.orthoframe({B, V})
        if self:Cosine_incidence(V1,H1) > 0 then -- second circular base visible
            table.append(rep.visible, {H+R*I, "m", H, V, "c"})
            table.insert(rep.section, {H+R*I, "m", H, V, "c"})
        else
            table.append(rep.hidden, {H+R*I, "m", H, V, "c"})
        end
        if self:Cosine_incidence(V1,O) < 0 then -- first circular base visible
            table.append(rep.visible, {B+R*I, "m", B, V, "c"})
            table.insert(rep.section, {B+R*I, "m", B, V, "c"})
        else
            table.append(rep.hidden, {B+R*I, "m", B, V, "c"})
        end
    end
    return rep
end


function ld.graph3d:Frustum_outline(B, R, r, V, H) -- or Frustum_outline(B, R, r, H)
-- returns a table:
-- {side=<path3>, section=<list of paths>, visible=<path>, hidden=<path>, tangency=<list of dots>, angle=<angle in degrees>}

    if R == r then
        return self:Cylinder_outline(B,R,V,H)
    end
    if not pt3d.isPoint3d(H) then -- format B, R, r, H
        H = V; V = H-B
    end
    if pt3d.dot(V,H-B) < 0 then V = -V end
    local U = H-B
    local rep = {}
    local sin, cos, pi = math.sin, math.cos, math.pi
    local O, I, J = ld.orthoframe({B, V})
    local mat, H1, V1 = self.matrix3d, H, V
    if not ld.isID3d(mat) then
        O, I, J = ld.mtransform3d(O,mat), ld.mLtransform3d(I,mat), ld.mLtransform3d(J,mat)
        H1 = ld.mtransform3d(H,mat); V1 = ld.mLtransform3d(V,mat)
    end
    local angle = self:Arg(self:Proj3d(H)-self:Proj3d(B))*ld.rad
    if angle < 0 then angle = angle+180
        elseif angle > 180 then angle = angle-180 
    end    
    local k = r/R
    local S = (H-k*B)/(1-k)
    local S1 = ld.mtransform3d(S,mat);
    rep.visible = {}
    rep.hidden = {}
    rep.side = {}
    rep.section = {}
    rep.angle = angle
    local f = function(t)
        local  A = O + R*(cos(t)*I+sin(t)*J)
        local N = self.Normal
        if ld.projection_mode == "central" then N = ld.camera-A end
        return pt3d.det(N, A-S1, R*(-sin(t)*I+cos(t)*J) )
    end
    local T = ld.solve(f,-pi,pi)
    if (T ~= nil) and (#T >= 2) then
        local M1, M2 = O+R*(cos(T[1])*I+sin(T[1])*J), O+R*(cos(T[2])*I+sin(T[2])*J)
        local t = (T[1]+T[2])/2
        local M3 = O+R*(cos(t)*I+sin(t)*J)
        if not ld.isID3d(mat) then
            M1, M2 = table.unpack( ld.mtransform3d({M1, M2}, ld.invmatrix3d(mat)) )
        end
        local M4, M5 = table.unpack( ld.scale3d({M1,M2}, k, S) )         
        rep.tangency = {M1,M2,M4,M5}
        local sens = 1
        if self:Cosine_incidence(M3-O,O) < 0 then sens = -1 end
        rep.visible = {M1, "m", M4, "l"}
        if self:Cosine_incidence(V1,H1) > 0 then -- second circular base visible
            table.append(rep.visible, {H, V, "c", M5, "m"})
            table.insert(rep.section, {M4,H,V,"c"})
        else
            table.append(rep.visible, {H, M5, r, sens, V, "ca"})
            table.append(rep.hidden, {M4, "m", H, M5, r, -sens, V, "ca"})
        end
        table.append(rep.visible, {M2, "l"})
        if self:Cosine_incidence(V1,O) < 0 then -- first circular base visible        
            table.append(rep.visible, {B, V, "c"})
            table.insert(rep.section, {M1,B,V,"c"})
        else -- first circular base not visible
            table.append(rep.visible, {B, M1, R, -sens, V, "ca"} )
            table.append(rep.hidden, {M2, "m", B, M1, R, sens, V, "ca"} )
        end
        rep.side = {M1,"m",M4,"l",H,M5,r,sens,V,"ca",M2,"l",B,M1,R,-sens,V,"ca"}
    else
        O, I, J = ld.orthoframe({B, V})
        if self:Cosine_incidence(V1,H1) > 0 then -- second circular base visible
            table.append(rep.visible, {H+r*I, "m", H, V, "c"})
            table.insert(rep.section, {H+r*I, "m", H, V, "c"})
            if r < R then
                rep.side = {B+R*I, "m", B, V, "c", H+r*I, "m", H, V, "c"}
                table.append(rep.visible, {B+R*I, "m", B, V, "c"})
            end
        elseif r < R then
            table.append(rep.hidden, {H+r*I, "m", H, V, "c"})
        end
        if self:Cosine_incidence(V1,O) < 0 then -- first circular base visible
            table.append(rep.visible, {B+R*I, "m", B, V, "c"})
            table.insert(rep.section, {B+R*I, "m", B, V, "c"})
            if r > R then
                rep.side = {H+r*I, "m", H, V, "c", B+R*I, "m", B, V, "c"}
                table.append(rep.visible, {H+r*I, "m", H, V, "c"})
            end
        elseif R < r then
            table.append(rep.hidden, {B+R*I, "m", B, V, "c"})
        end    
    end
    return rep
end

function ld.graph3d:Sphere_outline(C, R) -- C=center, R=radius
-- returns a table:
-- { visible=<path>, data=<{center,radius,normal}> }
    local O1, R1 = C, R
    local mat = ld.invmatrix3d( self.matrix3d )
    local N1 = ld.mLtransform3d(self.Normal,mat)
    if ld.projection_mode == "central" then
        local S = {C,R}
        local cam = ld.mtransform3d(ld.camera,mat)
        local S1 = { (cam+C)/2, pt3d.abs(C-cam)/2}
        O1, R1, N1 = ld.interSS(S,S1)
    end
    local O, I, J = ld.orthoframe({O1,N1})
    local rep = {}
    rep.visible = {O1+R1*I,O1,N1,"c"}
    rep.data = {O1,R1,N1}
    return rep
end

------------------------ tangency --------------------------------------

function ld.graph3d:Cone_tangency(B, R, V, H) -- or Cone_tangency(B, R, H)
-- returns a list of points
    local rep = self:Cone_outline(B, R, V, H)
    return rep.tangency
end


function ld.graph3d:Cylinder_tangency(B, R, V, H) -- or Cylinder_tangency(B, R, H)
-- returns a list of points
    local rep = self:Cylinder_outline(B, R, V, H)
    return rep.tangency
end

function ld.graph3d:Frustum_tangency(B, R, r, V, H) -- or Frustum_tangency(B, R, r, H)
-- returns a list of points
    local rep = self:Frustum_outline(B, R, r, V, H)
    return rep.tangency
end

function ld.graph3d:Sphere_tangency(C, R, P) -- C=center, R=radius, P=plane
-- returns a list of points
    local B, r, n = ld.interPS(P, {C,R})
    if B == nil then return end
    local sin, cos, pi = math.sin, math.cos, math.pi
    local O, I, J = ld.orthoframe({B,n})
    local mat, C1 = self.matrix3d, C
    local rep = {}
    local invmat = ld.invmatrix3d(mat)
    if not ld.isID3d(mat) then
        O, I, J = ld.mtransform3d(O,mat), ld.mLtransform3d(I,mat), ld.mLtransform3d(J,mat)
        C1 = ld.mtransform3d(C,mat)
    end
    local f = function(t)
        local A1 = O + r*(cos(t)*I+sin(t)*J)
        local A = ld.mtransform3d(A1,invmat)
        local N = self.Normal
        if ld.projection_mode == "central" then N = ld.camera-A1 end
        return pt3d.dot( ld.mLtransform3d(N, invmat), C-A)
    end
    local T = ld.solve(f,-pi,pi)
    if (T ~= nil) and (#T >= 2) then
        local M1, M2 = O+r*(cos(T[1])*I+sin(T[1])*J), O+r*(cos(T[2])*I+sin(T[2])*J)
        if not ld.isID3d(mat) then
            rep = ld.mtransform3d({M1, M2}, invmat)
        else
            rep = {M1, M2}
        end
    end
    return rep
end

------------------------------ drawing methods -------------------------

function ld.graph3d:Dcylinder(A,r,V,B,args) 
-- ou Dcylinder(A,r,B,args): cylindre droit de A vers B
-- ou Dcylinder(A,V,r,args): ancienne syntaxe, 
-- dessine un cylindre en fil de fer
-- A est le centre d'une face circulaire de rayon r orthogonale au vecteur V
-- l'autre face a pour centre B
-- args est une table à 6 champs :
-- {mode =0/1, hiddenstyle="dotted", hiddencolor = linecolor, edgecolor=linecolor, color="", opacity=1}
-- mode = 0 fil de fer
-- mode = 1 grille
-- color = "" : pas de remplissage, color ~= "" remplissage avec ball color
    if pt3d.isPoint3d(r) then -- ancienne syntaxe A,V,r,args
        local R = r
        r = V; V = R; args = B; B = A+V
    elseif not pt3d.isPoint3d(B) then -- syntaxe A,r,B,args
        args = B; B = V; V = B-A
    end
    args = args or {}
    args.color = args.color or ""
    args.color = self:Define_temp_color(args.color)
    args.edgecolor = args.edgecolor or self.param.linecolor
    args.edgestyle = args.edgestyle or self.param.linestyle
    args.edgewidth = args.edgewidth or self.param.linewidth
    args.hiddencolor = args.hiddencolor or args.edgecolor
    args.hiddenstyle = args.hiddenstyle or ld.Hiddenlinestyle
    --if not Hiddenlines then args.hiddenstyle = "noline" end
    args.mode = args.mode or 0
    args.opacity = args.opacity or 1
    args.gradsection = args.gradsection or {25,18,50}
    args.gradside= args.gradside or {50,10,100}
    if args.gradient == nil then args.gradient = true end
    local lsection, msection, rsection = table.unpack( args.gradsection)
    local lside, mside, rside = table.unpack( args.gradside)
    local gradStyleSide = "left color="..args.color.."!"..tostring(lside)..",right color = "..args.color.."!"..tostring(rside)..",middle color="..args.color.."!"..tostring(mside)
    local gradStyleSection = "left color="..args.color.."!"..tostring(lsection)..",right color = "..args.color.."!"..tostring(rsection)..",middle color="..args.color.."!"..tostring(msection)
    
    local oldfillstyle = self.param.fillstyle
    local oldfillopacity = self.param.fillopacity
    local oldfillcolor = self.param.fillcolor
    local oldlinestyle = self.param.linestyle
    local oldlineopacity = self.param.lineopacity
    local oldlinecolor = self.param.linecolor
    local oldlinewidth = self.param.linewidth
    local cyl = self:Cylinder_outline(A,r,V,B)
    local angle = cyl.angle
    if args.color ~= "" then  --fill side and sections
        if args.gradient then
            gradStyleSide = gradStyleSide..",shading angle="..ld.strReal(angle)
            gradStyleSection = gradStyleSection..",shading angle="..ld.strReal(angle)
            self:Filloptions("gradient", gradStyleSide,args.opacity)
        else
            self:Filloptions("full", args.color, args.opacity)
        end
        self:Linestyle("noline")
        self:Dpath3d(cyl.side)
        if args.gradient then self:Filloptions("gradient", gradStyleSection,args.opacity) end
        for _, p in ipairs(cyl.section) do
            self:Dpath3d(p)
        end
    end
    if args.mode ~= ld.mGrid then -- edges
        self:Filloptions("none")
        self:Lineoptions(args.edgestyle,args.edgecolor,args.edgewidth)
        self:Dpath3d(cyl.visible)
        if (args.hiddenstyle ~= "noline") then -- partie cachée
            self:Lineoptions(args.hiddenstyle,args.hiddencolor,args.edgewidth)
            self:Dpath3d(cyl.hidden)
        end
    else
        self:Dpoly(ld.cylinder(A,r,V,B,35,false), {mode=0,hiddenstyle=args.hiddenstyle, edgecolor=args.edgecolor,hiddencolor=args.hiddencolor, reverse=(self:Det3d()<0), edgestyle=args.edgestyle, edgewidth=args.edgewidth})
    end
    self:Filloptions(oldfillstyle,oldfillcolor,oldfillopacity)
    self:Lineoptions(oldlinestyle,oldlinecolor,oldlinewidth) 
    self:Lineopacity(oldlineopacity)
end


function ld.graph3d:Dcone(C,r,V,A,args) 
-- ou Dcone(C,r,A,args)
-- ou Dcone(A,V,r,args) (ancienne syntaxe)
-- dessine un cône en fil de fer
-- A est le sommet
-- le centre de la face circulaire de rayon r orthogonale au vecteur V est C
-- args est une table à 5 champs :
-- {mode =0/1, hiddenstyle="dotted", hiddencolor = linecolor, edgecolor= linecolor, color="", opacity=1}
-- mode = 0 fil de fer
-- mode = 1 grille
-- color = "" : pas de remplissage, color ~= "" remplissage avec gradient bi linéaire
    if pt3d.isPoint3d(r) then -- ancien format : sommet, vecteur, rayon, args (cône droit)
        args = A; A = C
        r, V = V, r
        C = A+V
    elseif not pt3d.isPoint3d(A) then -- format C,r,A,args (cône droit)
            args = A; A = V; V = A-C
    end
    args = args or {}
    args.color = args.color or ""
    args.color = self:Define_temp_color(args.color)
    args.edgecolor = args.edgecolor or self.param.linecolor
    args.edgestyle = args.edgestyle or self.param.linestyle
    args.edgewidth = args.edgewidth or self.param.linewidth
    args.hiddencolor = args.hiddencolor or args.edgecolor
    args.hiddenstyle = args.hiddenstyle or ld.Hiddenlinestyle
    if args.apex == nil then args.apex = true end
    --if not Hiddenlines then args.hiddenstyle = "noline" end
    args.mode = args.mode or 0
    args.opacity = args.opacity or 1
    args.gradsection = args.gradsection or {25,18,50}
    args.gradside= args.gradside or {50,10,100}
    if args.gradient == nil then args.gradient = true end
    local lsection, msection, rsection = table.unpack( args.gradsection)
    local lside, mside, rside = table.unpack( args.gradside)
    local gradStyleSide = "left color="..args.color.."!"..tostring(lside)..",right color = "..args.color.."!"..tostring(rside)..",middle color="..args.color.."!"..tostring(mside)
    local gradStyleSection = "left color="..args.color.."!"..tostring(lsection)..",right color = "..args.color.."!"..tostring(rsection)..",middle color="..args.color.."!"..tostring(msection)
    
    local oldfillstyle = self.param.fillstyle
    local oldfillopacity = self.param.fillopacity
    local oldfillcolor = self.param.fillcolor
    local oldlinestyle = self.param.linestyle
    local oldlineopacity = self.param.lineopacity
    local oldlinecolor = self.param.linecolor
    local oldlinewidth = self.param.linewidth
    local cone = self:Cone_outline(C,r,V,A)
    local angle = cone.angle
    if args.color ~= "" then  --fill side and sections
        if args.gradient then
            gradStyleSide = gradStyleSide..",shading angle="..ld.strReal(angle)
            gradStyleSection = gradStyleSection..",shading angle="..ld.strReal(angle)
            self:Filloptions("gradient", gradStyleSide,args.opacity)
        else
            self:Filloptions("full", args.color,args.opacity)
        end
        self:Linestyle("noline")
        self:Dpath3d(cone.side)
        if args.gradient then self:Filloptions("gradient", gradStyleSection,args.opacity) end
        for _, p in ipairs(cone.section) do
            self:Dpath3d(p)
        end
    end
    if args.mode ~= ld.mGrid then -- edges
        self:Filloptions("none")
        self:Lineoptions(args.edgestyle,args.edgecolor,args.edgewidth)
        self:Dpath3d(cone.visible)
        if (cone.tangency == nil) and (#cone.side > 0) and (args.apex) then
            self:Ddots3d(A, "scale=0.5")
        end
        if (args.hiddenstyle ~= "noline") then -- partie cachée
            self:Lineoptions(args.hiddenstyle,args.hiddencolor,args.edgewidth)
            self:Dpath3d(cone.hidden)
        end
    else
        self:Dpoly(ld.cone(C,r,V,A,35,false), {mode=0,hiddenstyle=args.hiddenstyle, edgecolor=args.edgecolor,hiddencolor=args.hiddencolor, reverse=(self:Det3d()<0), edgestyle=args.edgestyle, edgewidth=args.edgewidth})
    end
    self:Filloptions(oldfillstyle,oldfillcolor,oldfillopacity)
    self:Lineoptions(oldlinestyle,oldlinecolor,oldlinewidth) 
    self:Lineopacity(oldlineopacity)
end


function ld.graph3d:Dfrustum(A,R,r,V,B,args) -- ou Dfrustum(A,R,r,V,args) pour un cône droit 
-- frustum drawn without facets (tronc de cône)
-- dessine un tronc de cône en fil de fer
-- A est le centre de la face de rayon R
-- le centre de l'autre face  C=A+V et son rayon est r
-- args est une table à 5 champs :
-- {mode =0/1, hiddenstyle="dotted", hiddencolor = linecolor, edgecolor=linecolor, color="", opacity=1}
-- mode = 0 fil de fer
-- mode = 1 grille
-- color = "" : pas de remplissage, color ~= "" remplissage avec linéaire
    if R == r then -- cylinder
        if not isPoint3d(B) then self:Dcylinder(A,V,R,B) -- B is args in this case
        else self:Dcylinder(A,R,V,B,args)
        end
        return
    end
    if not pt3d.isPoint3d(B) then -- frustum(A,R,r,B,args)
        args = B
        B = V
        V = B-A
    end
    args = args or {}
    args.old = args.old or false
    if args.old then B = A+B end
    args.color = args.color or ""
    args.color = self:Define_temp_color(args.color)
    args.edgecolor = args.edgecolor or self.param.linecolor
    args.edgestyle = args.edgestyle or self.param.linestyle
    args.edgewidth = args.edgewidth or self.param.linewidth
    args.hiddencolor = args.hiddencolor or args.edgecolor
    args.hiddenstyle = args.hiddenstyle or ld.Hiddenlinestyle
    --if not Hiddenlines then args.hiddenstyle = "noline" end
    args.mode = args.mode or 0
    args.opacity = args.opacity or 1
    args.gradsection = args.gradsection or {25,18,50}
    args.gradside= args.gradside or {50,10,100}
    if args.gradient == nil then args.gradient = true end
    local lsection, msection, rsection = table.unpack( args.gradsection)
    local lside, mside, rside = table.unpack( args.gradside)
    local gradStyleSide = "left color="..args.color.."!"..tostring(lside)..",right color = "..args.color.."!"..tostring(rside)..",middle color="..args.color.."!"..tostring(mside)
    local gradStyleSection = "left color="..args.color.."!"..tostring(lsection)..",right color = "..args.color.."!"..tostring(rsection)..",middle color="..args.color.."!"..tostring(msection)
    
    local oldfillstyle = self.param.fillstyle
    local oldfillopacity = self.param.fillopacity
    local oldfillcolor = self.param.fillcolor
    local oldlinestyle = self.param.linestyle
    local oldlineopacity = self.param.lineopacity
    local oldlinecolor = self.param.linecolor
    local oldlinewidth = self.param.linewidth
    local frustum = self:Frustum_outline(A,R,r,V,B)
    local angle = frustum.angle
    if args.color ~= "" then  --fill side and sections
        if args.gradient then
            gradStyleSide = gradStyleSide..",shading angle="..ld.strReal(angle)
            gradStyleSection = gradStyleSection..",shading angle="..ld.strReal(angle)
            self:Filloptions("gradient", gradStyleSide,args.opacity)
        else
            self:Filloptions("full", args.color,args.opacity)
        end
        self:Linestyle("noline")
        self:Dpath3d(frustum.side)
        if args.gradient then self:Filloptions("gradient", gradStyleSection,args.opacity) end
        for _, p in ipairs(frustum.section) do
            self:Dpath3d(p)
        end
    end
    if args.mode ~= ld.mGrid then -- edges
        self:Filloptions("none")
        self:Lineoptions(args.edgestyle,args.edgecolor,args.edgewidth)
        self:Dpath3d(frustum.visible)
        if (args.hiddenstyle ~= "noline") then -- partie cachée
            self:Lineoptions(args.hiddenstyle,args.hiddencolor,args.edgewidth)
            self:Dpath3d(frustum.hidden)
        end
    else
        self:Dpoly(ld.frustum(A,R,r,V,B,35,false), {mode=0,hiddenstyle=args.hiddenstyle, edgecolor=args.edgecolor,hiddencolor=args.hiddencolor, reverse=(self:Det3d()<0), edgestyle=args.edgestyle, edgewidth=args.edgewidth})
    end
    self:Filloptions(oldfillstyle,oldfillcolor,oldfillopacity)
    self:Lineoptions(oldlinestyle,oldlinecolor,oldlinewidth) 
    self:Lineopacity(oldlineopacity)
end


function ld.graph3d:Dsphere(A,r,args)
-- dessine une sphère en fil de fer
-- A est le sommet, r le rayon
-- args est une table à 5 champs :
-- {mode=0/1/2, hiddenstyle="dotted", hiddencolor = linecolor, edgecolor=linecolor,color="", opacity=1}
-- color = "" : pas de remplissage, color ~= "" remplissage avec ball color
-- si mode 1 : edgestyle = linestyle, edgecolor = linecolor, edgewidth = linewidth
-- mode = 0 contour avec équateur
-- mode = 1 contour avec méridiens et fuseaux
-- mode = 2 contour seulement (cercle)
    args = args or {}
    args.color = args.color or ""
    args.edgecolor = args.edgecolor or self.param.linecolor
    args.hiddencolor = args.hiddencolor or args.edgecolor
    args.hiddenstyle = args.hiddenstyle or ld.Hiddenlinestyle
    --if not Hiddenlines then args.hiddenstyle = "noline" end
    args.edgestyle = args.edgestyle or self.param.linestyle
    args.edgecolor = args.edgecolor or self.param.linecolor
    args.edgewidth = args.edgewidth or self.param.linewidth    
    args.mode = args.mode or 0
    args.opacity = args.opacity or 1
    
    local oldfillstyle = self.param.fillstyle
    local oldfillopacity = self.param.fillopacity
    local oldfillcolor = self.param.fillcolor
    local oldlinestyle = self.param.linestyle
    local oldlineopacity = self.param.lineopacity
    local oldlinecolor = self.param.linecolor
    local oldlinewidth = self.param.linewidth
    self:Linecolor(args.edgecolor)
    local V = pt3d.vecK
    self:Filloptions("none","black")
    if args.color ~= "" then
        self:Filloptions("gradient", "ball color="..args.color, args.opacity)
    end
    --self:Dcircle(self:Proj3d(A),r)
    local L = self:Sphere_outline(A,r)
    self:Lineoptions(args.edgestyle,args.edgecolor,args.edgewidth)
    self:Dpath3d(L.visible)
    if  args.mode == 0 then -- équateur
        local M1, M2 = table.unpack( self:Sphere_tangency(A,r,{A,V}) )
        local M3 = ld.rotate3d(M1,90,{A,V})
        local sens
        local M4, I = table.unpack( ld.mtransform3d({M3,A}, self.matrix3d) )
        if self:Cosine_incidence(M4-I,M4) > 0 then sens = 1 else sens = -1 end
        self:Filloptions("none") --; self:Lineoptions(args.edgestyle,args.edgecolor,args.edgewidth)
        self:Darc3d(M1,A,M2,r,sens,V)
        self:Lineoptions(args.hiddenstyle,args.hiddencolor)
        self:Darc3d(M1,A,M2,r,-sens,V)
    elseif args.mode == 1 then -- grille
        self:Dpoly(ld.sphere(A,r),{mode=0,hiddenstyle=args.hiddenstyle,hiddencolor=args.hiddencolor,edgestyle=args.edgestyle,edgecolor=args.edgecolor,edgewidth=args.edgewidth,reverse=(self:Det3d()<0)})
    end
    self:Filloptions(oldfillstyle,oldfillcolor,oldfillopacity)
    self:Lineoptions(oldlinestyle,oldlinecolor,oldlinewidth); 
    self:Lineopacity(oldlineopacity)
end

function ld.graph3d:Dcut_sphere(C,R,P, options)
-- P is a plane, P = {A,n}
-- draw the sphere {C,R} cut by P (part in then half-plane containing n)
-- options: the same as for the Dsphere method plus options.visibletrace="" (contour of clip) and hiddentrace
    options.visibletrace = options.visibletrace or "" -- visible part of intersection
    options.hiddentrace = options.hiddentrace or ""
    local T = self:Sphere_tangency(C,R,P)
    local W, Vtrace, Htrace
    local O, R1, N = table.unpack( self:Sphere_outline(C,R).data )
    local c, r, n = ld.interPS(P, {C,R})
    if #T == 0 then
        local B = ld.proj3d(C,P)
        if pt3d.abs(B-C) > R then -- no intersection
            if pt3d.dot(B-C,P[2]) > 0 then self:Dsphere(C,R,options) end
            return 
        elseif pt3d.dot(C-B,P[2]) > 0 then
            if self:Cosine_incidence(n,c)>0 then
                W = ld.circle3db(O,R1,N)
                Htrace = ld.circle3db(c, r, n)
            else
                W = ld.concat( ld.circle3db(O,R1,N), ld.circle3db(c, r, n) )
                Vtrace = ld.circle3db(c, r, n)
            end
        else 
            if self:Cosine_incidence(n,c)>0 then
                W = ld.circle3db(c, r, n)
            end
            Vtrace = ld.circle3db(c, r, n)
        end
    else
        local A, B = table.unpack( T )
        c, r, n = ld.interPS(P, {C,R})
        if pt3d.det(A-c, N, P[2]) < 0 then A, B = B, A end
        local sens = 1
        if pt3d.det(B-O,P[2],N) < 0 then sens = -1 end
        W = {A,"m",c,B,r,1,n,"ca",O,A,R1,sens,N,"ca"}
        Vtrace = {A,"m",c,B,r,1,n,"ca"}
        Htrace = {A,"m",c,B,r,-1,n,"ca"}
    end
    if W ~= nil then
        self:Beginclip( W )
            self:Dsphere(C,R,options)
        self:Endclip()
    end
    if options.visibletrace ~= "" then
        self:Dpath3d(Vtrace, options.visibletrace)
    end
    if options.hiddentrace ~= "" then
        self:Dpath3d(Htrace, options.hiddentrace)
    end
end
