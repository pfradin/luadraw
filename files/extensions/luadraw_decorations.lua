-- luadraw_decorated.lua
-- date 2026/04/09
-- version 2.8
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   https://www.ctan.org/license/lppl

-- to decorate arcs (2D or 3D)

--------------------------------- arc 2D -------------------------------

function graph:Ddecoratedarc(B,A,C,r,sens,options)
    options = options or {}
    local sector_options = options.sector_options or "" -- fill or not the angular sector
    local arc_options = options.arc_options or "" -- draw_options for the arc
    local label = options.label or "" -- label text
    local node_options = options.node_options or "" -- node_options for the label
    local pos = options.pos or "auto" -- position of the label relative to the anchor point
    local dist = options.dist or r -- distance between the label and the center A
    local rotate = options.rotate or "none" -- or "auto" or "ortho", rotation of the label
    local angle = options.angle or 0 -- angle to turn the anchor point around the center A (initially, the anchor point is located on the arc and the bisector line)
    local ticks = options.ticks or "none" -- or {nb, length, space, draw_options}, to draws marks along the arc (acute angle)
    local showanchor = options.showanchor or false -- show the anchor point, the bisector line and the frame of the label
    
    local S = arcb(B,A,C,r,sens) -- arc with Bézier curves
    if S == nil then return end
    local dir = {}
    if sector_options ~= "" then
        self:Dpath(concat(S,{A,"l","cl"}),"draw=none,"..sector_options)
    end
    self:Dpath(S,arc_options)
    if label ~= "" then
        local b, c = A+r*cpx.normalize(B-A), A+r*cpx.normalize(C-A)
        local u = cpx.normalize((c+b)/2-A)
        local direct = 1
        if cpx.det(B-A,C-A) <= 0 then direct = -1 end
        u = direct*sens*u
        local anchor = A+dist*u
        if angle ~=0 then anchor = rotate(anchor,angle,A) end
        if rotate == "ortho" then 
            if u.im < 0 then u = -u end
            dir = {-cpx.I*u, u} 
        elseif rotate == "auto" then
            if u.re < 0 then u = -u end
            dir = {u, cpx.I*u} 
        end
        if pos == "auto" then 
            if rotate == "ortho" then 
                pos = self:Poslab(anchor-A, self:Arg(-cpx.I*u)*rad) 
            elseif rotate == "auto" then
                pos = self:Poslab(anchor-A, self:Arg(u)*rad) 
            else pos = self:Poslab(anchor-A,0)
            end 
        end
        if showanchor then
            self:Dseg({A,anchor},"dashed"); self:Ddots(anchor)
            self:Dlabel(label, anchor, {pos=pos, node_options=node_options..",draw", dir=dir})
        else
            self:Dlabel(label, anchor, {pos=pos, node_options=node_options, dir=dir})
        end
    end
    if type(ticks) == "number" then ticks = {ticks} end
    if type(ticks) == "table" then
        local n, length, space, d_options = table.unpack(ticks)
        self:Dmarkarc(B,A,C,r,n,length,space,d_options)
    end
end

---------------------- arc 3D ------------------------------------------

function graph:Ddecoratedarc3d(B,A,C,r,sens,options)
    options = options or {}
    local u, v = B-A, C-A
    local n = pt3d.prod(B-A,C-A)
    if pt3d.N1(n) < 1e-8 then -- n is null
        n = options.normal
        if n == nil then 
            print("Ddecoratedarc3d options normal is missing")
            return
        end
    end
    local sector_options = options.sector_options or "" -- fill or not the angular sector
    local arc_options = options.arc_options or "" -- draw_options for the arc
    local label = options.label or "" -- label text
    local node_options = options.node_options or "" -- node options for the label
    local pos = options.pos or "auto" -- position of the label relative to the anchor point
    local dist = options.dist or r -- distance between the label and the center A
    local rotate3D = options.rotate3d or "none" -- or "auto" or "ortho", rotation of the label in (BAC) plane
    local rotate = options.rotate or "none" -- or "auto" or "ortho", rotation of the label on the screen plane
    local angle = options.angle or 0 -- angle to turn the anchor point around the center A (initially, the anchor point is located on the arc and the bisector line)
    local ticks = options.ticks or "none" -- or {nb, length, space, draw_options}, to draws marks along the arc (acute angle)
    local showanchor = options.showanchor or false -- show the anchor point, the bisector line and frame of the label
    local bezier = options.bezier -- use bezier curves for the arc (or polyline if bezier=false)
    if bezier == nil then bezier = true end
    
    local S = arc3db(B,A,C,r,sens,n) -- arc with Bézier curves
    if S == nil then return end
    local dir = {}
    if sector_options ~= "" then
        self:Dpath3d(concat(S,{A,"l","cl"}),"draw=none,"..sector_options)
    end
    if bezier then self:Dpath3d(S,arc_options)
    else self:Dpolyline3d(path3d(S),arc_options)
    end
    if label ~= "" then
        local b, c = A+r*pt3d.normalize(B-A), A+r*pt3d.normalize(C-A)
        local u = pt3d.normalize((c+b)/2-A)
        u = sens*u
        local v = rotate3d(u,90,{Origin,n})
        local w = rotate3d(u,-90,{Origin,n})
        local anchor = A+dist*u
        if angle ~= 0 then anchor = rotate3d(anchor,angle,{A,n}) end
        local U, V = self:Proj3dV(anchor-A), self:Proj3dV(w)
        local angle2D = 0
        if rotate3D == "ortho" then 
            if self:Cosine_incidence(n,anchor) < 0 then w = -w; V = -V end
            if V.re < 0 then dir = {-w, -u}; V = -V else dir = {w,u} end
        elseif rotate3D == "auto" then
            if self:Cosine_incidence(n,anchor) < 0 then v = -v end
            if self:Proj3dV(u).re < 0 then dir = {-u,-v}; u = -u else  dir = {u,v} end
        elseif rotate == "ortho" then
            angle2D = self:Arg(V)*rad
            if angle2D < -90 then angle2D = angle2D+180
            elseif angle2D > 90 then angle2D = angle2D-180
            end
        elseif rotate == "auto" then
            angle2D = self:Arg(U)*rad
            if angle2D < -90 then angle2D = angle2D+180
            elseif angle2D > 90 then angle2D = angle2D-180
            end
        end
        if pos == "auto" then 
            if rotate3D == "ortho" then 
                pos = self:Poslab(U, self:Arg(V)*rad) 
            elseif rotate3D == "auto" then
                pos = self:Poslab(U, self:Arg(self:Proj3dV(u))*rad) 
            else pos = self:Poslab(U,angle2D)
            end 
        end
        if angle2D ~= 0 then node_options = node_options..",rotate="..angle2D end
        if showanchor then
            self:Dseg3d({A,anchor},"dashed"); self:Ddots3d(anchor) 
            self:Dlabel3d(label, anchor, {pos=pos, node_options=node_options..",draw", dir=dir})
         else
            self:Dlabel3d(label, anchor, {pos=pos, node_options=node_options, dir=dir})
         end
    end
    if type(ticks) == "number" then ticks = {ticks} end
    if type(ticks) == "table" then
        local nb, length, space, d_options = table.unpack(ticks)
        local u, w = pt3d.normalize(B-A), C-A
        local v = pt3d.prod(n,u)
        local mat = { self:Proj3d(A), self:Proj3dV(u), self:Proj3dV(v) }
        self:Savematrix()
        self:Composematrix(mat)
        local a, b, c = 0, 1, Z( pt3d.dot(w,u), pt3d.dot(w,v) )
        self:Dmarkarc(b,a,c,r,nb,length,space,d_options)
        self:Restorematrix()
    end
end
