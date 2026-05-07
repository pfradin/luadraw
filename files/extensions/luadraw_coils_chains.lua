-- luadraw_coils_chains.lua 
-- date 2026/05/07
-- version 3.0
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   https://www.ctan.org/license/lppl


-- draw coils and chains
local ld = luadraw
local graph = ld.graph
local cpx = ld.cpx
local Z = cpx.Z

function graph:Dcoil(list,R,options)
-- list = {start, nb1, end1, nb2, end2, ...} or list = {polygonale line, nb turns}
-- R = radius
-- options={ color="gray", colorB = color, reverse=false, border=current color, border_width=current lien width, holes=false, start_angle=nil, end_angle=nil, tension=1, wire_dia=2R/15, direction=1, leftC=100, midC=10, rightC=50 }

    options = options or {}
    local color = options.color or "gray"
    local colorB = options.colorB or color
    local mirror = options.reverse or false
    local border = options.border or self.param.linecolor
    local border_width = options.border_width or self.param.linewidth
    local wireframe = options.wireframe or false
    local curve = (#list == 2) and (type(list[1]) == "table") and (type(list[2]) == "number")
    local holes = options.holes or false
    local tension = options.tension or 1
    local alpha1 = options.start_angle
    local alpha2 = options.end_angle
    local alphaA, alphaB = alpha1, alpha2
    local d = options.wire_dia or 2*R/15
    local sens = options.direction or 1
    local leftC = options.leftC or 100
    local midC = options.midC or 10
    local rightC = options.rightC or 50 
    d = d/2
    if mirror then
        list = ld.reverse(list)
    end
    if curve then
        local n = table.remove(list)
        list = list[1]
        if not cpx.isComplex(list[1]) then list = list[1] end -- first component
        local f, Len = ld.curvilinear_param(list)
        local L1 = {list[1]}
        for k = 1, n do
            table.insert(L1,1); table.insert(L1,f(k/n))
        end
        list = L1
    end
    local oldcolor = self.param.linecolor
    local oldwidth = self.param.linewidth
    local oldstyle = self.param.linestyle
    self:Lineoptions("solid",border,border_width)
    
    local Dsegaux = function(S, r, endb, back, start)
    -- endb = 1 -> round, endb = 0-> open, endb = 2-> butt, endb = 3 -> round at start or end
    -- back = true/false
    -- start = true/false
        local options ="left color ="..color.."!"..leftC..",right color="..color.."!"..rightC..",middle color="..color.."!"..midC
        if (endb ~= 1)  then
            if start then
                options = "left color ="..color.."!"..midC..",right color="..color.."!"..leftC
            else
                options = "left color ="..color.."!"..midC..",right color="..color.."!"..tostring(leftC/2)
            end
        end
        local ep = sens*tension*R/25
        if back then
            options = "left color ="..colorB.."!80,right color="..colorB.."!15"
        else ep =-ep
        end
        if endb ~= 1 then ep = 0 end
        local a, b = table.unpack(S)
        local u = cpx.normalize(b-a)
        local v = cpx.I*u
        local c = (a+b)/2+ep*v
        local u1, u2 = c-a, c-b
        local v1, v2, C = r*cpx.I*cpx.normalize(u1), -r*cpx.I*cpx.normalize(u2) 
        if tension ~= 0 then
            C = {a+v1,"m",a+v1+u1, b+v2+u2, b+v2,"b"}
        else
            C = {a+v1, "m", b+v2,"l"}
        end
        ld.insert(C, {b,b-v2,r,-1,"ca",b-v2+u2,a-v1+u1,a-v1,"b"})
        if endb%2 == 1 then ld.insert(C,{a,a+v1,r,-1,"ca"}) end
        if endb == 2 then table.insert(C,"cl") end
        local angle = (cpx.arg(v)*ld.rad )--%180
        if wireframe then
            local col = color
            if back then col= colorB end
            self:Dpath(C, "fill="..col)
        else
            self:Dpath(C, options..",shading angle="..ld.strReal(angle))
        end
    end 

    local B, A, C, n = list[1]
    local close = cpx.abs(B-list[#list]) < 1e-4
    local first = true
    local Lb, Lf, Ends, Holes = {}, {}, {}, {}
    local nb = #list
    for i = 2, nb-1, 2 do
        A = B; B = list[i+1]; n = list[i]; --alphA = alphaB 
        if i > 3 then
            C = list[i-3]; 
            local theta = cpx.angle(B-A,A-C)*ld.rad
            if math.abs(theta) < 0.1 then
                alphaA = alphaB+ theta
                alphaB = 0
            end
        else 
            if alphaA == nil then alphaA = math.atan(cpx.abs(B-A)/(4*R*n))*ld.rad end
            if alphaB == nil then alphaB = alphaA end
        end
        local t = cpx.normalize(B-A)
        local distA = R*math.tan(alphaA*ld.deg)
        local distB = R*math.tan(alphaB*ld.deg)
        local A1,B1 = A+distA*t, B-distB*t
        local a, u, b = A1, (B1-A1)/(2*n-1), B1
        local v = -sens*R*cpx.I*t
        if #Ends > 0 then
            local S = table.remove(Ends)
            table.insert(Lf,{A1+v,S[2]})
        else 
            table.insert(Ends,{A,A1+v})
        end
        if first then 
             first = false
            if holes then table.insert(Holes, A) end
        end
        for k = 1, n do
            table.insert(Lb, {a+v, a-v+u}); a = a+2*u
            if k <= n-1 then table.insert(Lf, {a+v, a-v-u}); end
        end
        table.insert(Ends,{B,B1-v})
        if i == nb-1 then
            if holes then table.insert(Holes, B) end
        end
    end
    -- drawing
    for k, S in ipairs(Lb) do
        Dsegaux(S, d, 1, true, false) -- true: back, false: start
    end
    if close then
        local S1, S2 = table.remove(Ends,1), table.remove(Ends)
        table.insert(Lf, {S1[2],S2[2]})
    end
    for k, S in ipairs(Ends) do
        if k == 1 then Dsegaux(S, d, 3, false, true)
        elseif k == #Ends then
             Dsegaux(S, d, 3, false, false)
        else Dsegaux(S, d, 0, false, true)
        end
    end
    for k, S in ipairs(Lf) do
        Dsegaux(S, d, 1, false, false)
    end
    for _, C in ipairs(Holes) do
        if holes then self:Dcircle(C,d/2,"draw=none,fill=black") end
    end
    self:Lineoptions(oldstyle, oldcolor, oldwidth)
end


local coil2 = function(list, R, direction)
    direction= direction or 1
    local end_angle = 0
    
    local spire_base = function(angle)
        angle = angle or 0
        local a, a1, a2 = Z(0,0), Z(0,1.44), Z(0.555,2)
        local b, b1, b2 = Z(1,2), Z(1.455,2), Z(2,1.44)
        local c, c1, c2 = Z(2,0), Z(2,-1.11), Z(1.55,-2)
        local d, d1, d2 = Z(1,-2), Z(0.455,-2), Z(0,-1.11)
        local mat = {Z(0,0), Z(0.5,0), Z(0,1) }
        local k = 1/math.sqrt(2)/2
        a1,a2,b,b1,b2,c,c1,c2,d,d1,d2 = table.unpack( ld.mtransform({a1,a2,b,b1,b2,c,c1,c2,d,d1,d2}, mat) )
        local ap = a
        c1, c2 , d, d1, d2, ap = table.unpack( ld.map(function(z) z=cpx.toComplex(z); return Z( k*(z.re-1)+1,z.im) end, {c1,c2,d,d1,d2,a}) )
        if angle ~= 0 then
            local mat2 = ld.matrixof( function(z) return ap+(z-ap)*cpx.exp(cpx.I*angle*2/3) end )
            a2,b,b1,b2,c,c1,c2,d,d1,d2 = table.unpack( ld.mtransform({a2,b,b1,b2,c,c1,c2,d,d1,d2}, mat2) )
        end
        return {a,"m",a1,a2,b, "b", b1, b2, c,"b", c1, c2, d, "b", d1, d2, ap, "b"}
    end
    
    local spring = function(A, nb, B)
        local Len = cpx.abs(B-A)
        local angle = cpx.arg(B-A)
        local sc = R/2
        local sp = spire_base(end_angle)
        local tr = sp[17].re*sc
        local mat = {Z(0,0), Z(sc,0), Z(0,sc)}
        local tr_mat = {Z(tr,0), Z(1,0), Z(0,1)}
        local Len_mat = {Z(0,0), Z(Len/(nb*tr),0), Z(0,1)}
        local rot_mat = ld.matrixof( function(z) return A+cpx.exp(cpx.I*angle)*z end )
        sp = ld.mtransform(sp, mat)
        local rep = {sp}
        for i = 2, nb do
            sp = ld.mtransform(sp, tr_mat)
            table.insert(rep, sp)
        end
        rep = ld.mtransform(rep,Len_mat)
        return ld.mtransform(rep,rot_mat)
    end

    local ret = {}
    local curve = (#list == 2) and (type(list[1]) == "table") and (type(list[2]) == "number")
    if curve then
        local n = table.remove(list)
        list = list[1]
        if not cpx.isComplex(list[1]) then list = list[1] end -- first component
        local f, Len = ld.curvilinear_param(list)
        local L1 = {list[1]}
        for k = 1, n do
            table.insert(L1,1); table.insert(L1,f(k/n))
        end
        list = L1
    end
    local N = #list
    local B, A, C = list[1]
    for i = 2, N-1, 2 do
        A = B
        B, sp = list[i+1], list[i] 
        if i <= N-3 then
            C = list[i+3]
            end_angle = cpx.angle(B-A,C-B)
        else end_angle = 0
        end
        ld.insert(ret, spring(A, sp, B))
    end
    if direction == 1 then ret = ld.reverse(ret) end
    return ret
end


function graph:Dcoil2(list,R,options)
-- list = {start, nb1, end1, nb2, end2, ...} or list = {polygonale line, nb turns}
-- R = radius
-- options={ color=linecolor, border=nil ("color"), border_width=linewidth,width=linewidth, direction=1 }
    options = options or {}
    local color = options.color or self.param.linecolor
    local border = options.border or "white"
    local width = options.width or self.param.linewidth
    local border_width = options.border_width or self.param.linewidth
    local sens = options.direction or 1
    local draw_options
    local curve = (#list == 2) and (type(list[1]) == "table") and (type(list[2]) == "number")
    if border ~= "" then
        draw_options = "line width="..tostring(border_width/10).."pt,double="..color..",draw="..border..",double distance="..tostring(width/10).."pt"
    else
        draw_options = "color="..color..",line width="..tostring(width/10).."pt"
    end
    draw_options = draw_options..",line cap=butt"
    local lst = coil2(list,R,sens)
    for _, sp in ipairs(lst) do
        self:Dpath(sp,draw_options)
    end
end

--------------------- chains -------------------------------------------

local chain = function(list, rx, ry, ep)  
    local f, Len = ld.curvilinear_param(list)
    local n = math.floor( (Len/rx+1)/2 )
    n = 2*n-1
    rx = Len/n/2
    if ry == "circle" then ry = rx
    elseif ry == nil then ry = rx/1.618
    else ry = ry/2
    end
    ep = ep or ry/2

    local mailleA = function(a,b)
        local u = b-a
        local r = cpx.abs(u)/2
        u = cpx.normalize(u)
        local c, alpha = (a+b)/2, cpx.arg(b-a)*ld.rad 
        return {a, "m", c, r, ry, alpha, "e", a-ep*u, "m", c, r+ep, ry+ep, alpha, "e"}
    end
    
    local mailleB = function(a,b)
        local v = ep*cpx.I*cpx.normalize(b-a)/2
        local alpha = cpx.arg(b-a)*ld.rad
        return {a+v, "m", b+v, "l", b, b-v, ep, ep/2, -1, alpha, "ea", a-v, "l", a, a+v, ep, ep/2, -1, alpha, "ea", "cl"}
    end
    
    local back, front = {}, {}
    local x2, x1 = f(0)
    for k = 1, n do
        x1 = x2; x2 = f(k/n)
        if k%2 == 1 then -- mailleA
            table.insert(back,mailleA(x1,x2))
        else
            table.insert(front,mailleB(x1,x2))
        end
    end
    return back, front
end

function graph:Dchain(list, link_length, options)
-- list = list of complex numbers
    options = options or {}
    local rx = link_length
    local ry = options.width
    local ep = options.wire_dia
    local wireframe = options.wireframe or false
    local colorA = options.color or "gray"
    local colorB = options.colorB or colorA
    local border = options.border or self.param.linecolor
    local border_width = options.border_width or self.param.linewidth
    local leftC = options.leftC or 100
    local midC = options.midC or 10
    local rightC = options.rightC or 50 
    
    local oldfillstyle = self.param.fillstyle
    local oldfillopacity = self.param.fillopacity
    local oldfillcolor = self.param.fillcolor
    local oldcolor = self.param.linecolor
    local oldwidth = self.param.linewidth
    local oldstyle = self.param.linestyle
    self:Lineoptions("solid",border,border_width)
    local gradoptions = "left color="..colorA.."!"..leftC..",right color="..colorB.."!"..rightC..",middle color="..colorA.."!"..midC
    local bck, frt = chain(list,rx,ry,ep)
    for _, bc in ipairs(bck) do
        local angle = bc[6]
        if wireframe then
            self:Dpath(bc, "even odd rule, fill="..colorA)
        else
            self:Dpath(bc, "even odd rule,"..gradoptions..",shading angle="..(angle+90))
        end
    end
    for _, bc in ipairs(frt) do
        local angle = bc[10]
        if wireframe then
            self:Dpath(bc, "fill="..colorA)
        else
            self:Dpath(bc, gradoptions..",shading angle="..(angle+90))
        end
    end
    self:Lineoptions(oldstyle, oldcolor, oldwidth)
end


local chain2 = function(L,h)
-- a and b are two 2d points (complex numbers)
-- h = height  of wave
-- returns a back and front (two lists of paths)
    
    local f, Len = ld.curvilinear_param(L)
    local closed = cpx.abs(f(1)-f(0))<1e-4
    h = h or 0.0625
    local n = math.floor(Len/(4*h)) -- number of zigzag
    local back,front = {}, {} -- to build path
    local lastback, lastfront = {f(0)}, {f(0)}
    local x2, x1, c, d = f(0)
    for k = 1, n do
        x1 = x2; x2 = f(k/n)
        local w = (x2-x1)/12
        local v = h*cpx.I*cpx.normalize(x2-x1)
        c, d = (3*x1+x2)/4, (x1+3*x2)/4
        ld.insert(lastfront, {c-w+v,c-w+v,c+v,"b"})
        if (k > 1) or closed then 
            table.insert(front, lastfront)
        else
            table.insert(back, lastfront)
        end
        table.insert(front, {c-v,"m",c-v+w,d+v-w,d+v,"b"})
        lastfront = {d-v,"m",d-v+w,d-v+w,x2,"b"}
        
        ld.insert(lastback, {c-v-w,c-v-w,c-v,"b"})
        table.insert(back, lastback)
        table.insert(back, {c+v,"m",c+v+w,d-v-w,d-v,"b"})
        lastback = {d+v,"m",d+v+w,d+v+w,x2,"b"}
    end
    if closed then
        table.remove(back[1],1)
        back[1] = ld.concat(lastback, back[1])
        table.remove(front[1],1)
        front[1] = ld.concat(lastfront, front[1])
    else
        table.insert(back, lastback)
        table.insert(back, lastfront)
    end
    return back,front
end


function graph:Dchain2(L,R,options)
-- list = list of complex numbers
-- options={ color=linecolor, border=nil ("color"), border_width=4,width=linewidth, direction=1 }
    local h = R
    options = options or {}
    local color = options.color or self.param.linecolor
    local border = options.border or "white"
    local width = options.width or self.param.linewidth
    local border_width = options.border_width or self.param.linewidth
    local sens = options.direction or 1
    local back_options, front_options
    back_options = "color="..color..",line width="..tostring(width/10).."pt"
    if border ~= "" then
        front_options = "line width="..tostring(border_width/10).."pt,double="..color..",draw="..border..",double distance="..tostring(width/10).."pt"
    else
        front_options = back_options
    end
    local bck, frt = chain2(L,h)
    for _,bc in ipairs(bck) do
        self:Dpath(bc, back_options..",line cap=round")
    end
    for _, fr in ipairs(frt) do
        self:Dpath(fr,front_options..",line cap=butt")
    end
end
