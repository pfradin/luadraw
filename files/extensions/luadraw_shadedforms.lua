-- luadraw_shadedforms.lua 
-- date 2026/02/17
-- version 2.6
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.


-- to draw shaded polylines, shaded rectangles, shaded regions, color bars

require 'luadraw_palettes'

function graph:Dshadedpolyline(L,pal,options)
-- L is a list of complex numbers or a list of list od complex numbers
-- pal refers to a color palette (list of {r,g,b})
-- options is a table of parmaeters :
    -- values = "x" (values is "x" or "y" or a function f:(x,y)->f(x,y), applied at each point of L)
    -- width = current line width (expressed in tenths of a point)
    -- close = false (boolean indicating whether the line should be close)
    -- clip = {x1,x2,y1,y2} clipping window, or nil for the default window
    options = options or {}
    local values = options.values or "x"
    local wd = options.width or self.param.linewidth
    local close = options.close or false
    local clip = options.clip or nil
    local x1,x2,y1,y2
    if clip ~= nil then x1,x2,y1,y2 = table.unpack(clip) end
    local ep = wd/20*pt
    local f -- function applied at each point of L
    if values == "x" then f = function(x,y) return x end 
    elseif values == "y" then f = function(x,y) return y end 
    else f = values
    end
    
    if (L == nil) or (type(L) ~= "table") then return end
    local i = cpx.I

    if (type(L[1]) == "number") or isComplex(L[1]) then L = {L} end
    if clip ~= nil then
        L = clippolyline(table.copy(L),x1,x2,y1,y2,close)
    end
    local mat = self.matrix
    local transf
    if isID(mat) then transf = function(l) return l end
    else transf = function(l) return mtransform(l,mat) end
    end
    self:Savematrix(); self:IDmatrix()
    local bord, aux, a, b, c, u, v, w, color_index
    
    for _, cp1 in ipairs(L) do
        local cp = table.copy(cp1)
        a, b = cp[1], cp[2]
        while a == b do table.remove(cp,1); b = cp[2] end
        local cycle = (a==cp[#cp])
        close = close or cycle
        table.remove(cp,1); table.remove(cp,1)
        if close then
            if not cycle then table.insert(cp,a) end
            a = (a+b)/2
            table.insert(cp,a); table.insert(cp1,a)
        end
        local Min, Max = math.huge, -math.huge
        for _,z in ipairs(cp1) do
            local v = f(z.re,z.im)
            if v < Min then Min = v end
            if v > Max then Max = v end
        end
        local m2, m1, angle = f(a.re,a.im)
        if Max == Min then color_index = 0 else  color_index = (m2-Min)/(Max-Min) end
        local c2, c1 = palette(pal,color_index)
        v = cpx.normalize(b-a)
        bord = {a-ep*i*v,a+ep*i*v}
        aux = bord
        c = b; b = a
        for _, z in ipairs(cp) do
            angle = self:Arg(v)*rad+90
            a = b; b = c; c = z; u  =-v; v = cpx.normalize(c-b)
            if v == nil then
                c = b; b = a; v = -u
            else
                w = cpx.normalize(u+v)
                if w == nil then
                    bord = {b+ep*i*u, b-ep*i*u}
                else
                    bord = projO( bord,{b,w},u)
                end
                m1 = m2; m2 = f(b.re,b.im)
                if Max == Min then color_index = 0 else  color_index = (m2-Min)/(Max-Min) end
                c1 = c2; c2 = palette(pal,color_index)
                self:Dpolyline( transf(concat(reverse(aux),bord)), true, "draw=none,left color="..c1..",right color="..c2..",shading angle="..strReal(angle))
                aux = bord
                self:Dseg( transf(bord), "arrows=-,line width=0.1pt,color="..c2) -- pour masquer les séparations
            end
        end
        --last one
        m1 = m2; m2 = f(c.re,c.im)
        if Max == Min then color_index = 0 else color_index = (m2-Min)/(Max-Min) end
        c1 = c2; c2 = palette(pal, color_index)
        angle = self:Arg(v)*rad+90
        bord = {c-ep*i*v, c+ep*i*v}
        self:Dpolyline( transf(concat(reverse(aux),bord)), true,
           "draw=none,left color="..c1..",right color="..c2..",shading angle="..strReal(angle))
    end
    self:Restorematrix()
end

function graph:Dcolorbar(A,pal,options)
    -- A = reference point
    -- pal = color palette (list of {r,g,b})
    -- options = { minmax={0,1}, dir = cpx.I, width=0.5, len=8, values=0, addvalues=nil, digit=2, labelpos="E"} 
    options = options or {}
    local minmax = options.minmax or {0,1}
    local min, max = table.unpack(minmax)
    local dir = options.dir or cpx.I
    dir = cpx.normalize(dir)
    local wd = options.width or 0.5
    local len = options.length or 8
    local values = options.values or 0
    local digit = options.digits or 2
    local labelpos = options.labelpos or "E"
    local addvalues = options.addvalues
    if addvalues ~= nil then
        table.insert(addvalues,1,min); table.insert(addvalues,max)
        values = addvalues
    end
    if type(values) == "number" then
        values = linspace(min,max,values)
    else -- table
        table.sort(values)
    end
    local L = len*dir
    local h = wd*cpx.I*dir
    local N = #pal
    local dl = L/(N-1)
    local a = A
    local shadingangle = self:Arg(L)*rad+90
    local colorB, colorA = rgb(pal[1])
    local anchors = map(function(v) return A+L*(v-min)/(max-min) end, values)
    for k = 1, N-1 do
        colorA = colorB; colorB = rgb(pal[k+1])
        self:Drectangle(a,a+h,a+h+dl,"line width=0.1pt,color="..colorA..",left color="..colorA..",right color="..colorB..",shading angle="..shadingangle)
        a = a+dl
    end
    local labels, seg = {}, {}
    for k = 1, #values do
        insert(labels,{"$"..num(values[k],digit).."$", anchors[k], {pos=labelpos}})
        table.insert(seg, {anchors[k], anchors[k]+h})
    end
    self:Drectangle(A,A+L,A+h+L); self:Dpolyline(seg); self:Dlabel(table.unpack(labels))
end


function graph:Dshadedrectangle(x1,x2,y1,y2,pal,options)
-- pal = color palette
-- options = {grid={15,15}, values=|z| (function), bar="none", bardist=1, baroptions={}, out=nil}
-- values is a function (x,y) -> values(x,y) in R
-- this function needs \usepackage{shadings}
    options = options or {}
    local grid = options.grid or {15,15}
    local f = options.values or function(x,y) return cpx.abs(Z(x,y)) end
    local out = options.out
    local bar = options.bar or "none" -- "none" or "left" or "right" or "top" or "bottom"
    local bardist = options.bardist or 1
    local baroptions = options.baroptions or {}
    local deltaX, deltaY = (x2-x1)/grid[1], (y2-y1)/grid[2]
    local values, key = {}
    local vectors, v = {}
    local Min, Max = math.huge, -math.huge
    for i = 0, grid[1] do
        local x = x1+deltaX*i
        local x1 = x+deltaX/2
        for j = 0, grid[2] do
            local y = y1+deltaY*j
            local y1 = y+deltaY/2
            key = i.."/"..j
            local img = f(x,y)
            if img < Min then Min = img end
            if img > Max then Max = img end
            values[key] = img
        end
    end
    if out~= nil then table.insert(out,Min); table.insert(out,Max) end
    baroptions.minmax = {Min, Max}
    baroptions.values = baroptions.values or #pal
    local left_colors, right_colors, color = {}, {}
    for j = 0, grid[2] do 
        key = "0/"..j
        color = palette(pal,(values[key]-Min)/(Max-Min))
        table.insert(right_colors, color)
    end
    -- paint
    local eps =  1e-2 -- to widen the squares a little
    for i = 0, grid[1]-1 do
        local x = x1+deltaX*i
        left_colors = right_colors
        right_colors = {}
        for j = 0, grid[2] do 
            key = (i+1).."/"..j
            color = palette(pal,(values[key]-Min)/(Max-Min))
            table.insert(right_colors, color)
        end
        for j = 0, grid[2]-1 do
            local y = y1+deltaY*j
            local color0 = left_colors[j+1]
            local color1 = right_colors[j+1]
            local color2 = right_colors[j+2]
            local color3 = left_colors[j+2]
            local options = "upper left="..color3..",upper right="..color2..",lower left="..color0..",lower right="..color1
            self:Dpolyline({Z(x-eps,y-eps),Z(x+deltaX+eps,y-eps), Z(x+deltaX+eps,y+deltaY+eps), Z(x-eps,y+deltaY+eps)}, "draw=none,"..options)
        end
    end
    if bar == "right" then 
        baroptions.length = baroptions.length or 0.75*(y2-y1)
        baroptions.dir = cpx.I
        local ydep = y1+( (y2-y1) - baroptions.length )/2
        baroptions.width = baroptions.width or 0.5
        baroptions.width = math.abs( baroptions.width )
        baroptions.labelpos = baroptions.labelpos or "E"
        self:Dcolorbar(Z(x2+bardist+baroptions.width,ydep),pal, baroptions)
    elseif bar == "left" then
        baroptions.length = baroptions.length or 0.75*(y2-y1)
        baroptions.dir = cpx.I
        local ydep = y1+( (y2-y1) - baroptions.length )/2
        baroptions.width = baroptions.width or -0.5 
        baroptions.width = -math.abs( baroptions.width )
        baroptions.labelpos = baroptions.labelpos or "W"
        self:Dcolorbar(Z(x1-bardist+baroptions.width,ydep),pal, baroptions)
    elseif bar == "bottom" then
        baroptions.length = baroptions.length or 0.75*(x2-x1)
        baroptions.dir = 1
        local xdep = x1+( (x2-x1) - baroptions.length )/2
        baroptions.width = baroptions.width or 0.5;
        baroptions.width = math.abs( baroptions.width )
        baroptions.labelpos = baroptions.labelpos or "S"
        self:Dcolorbar(Z(xdep,y1-bardist-baroptions.width),pal, baroptions)
    elseif bar == "top" then
        baroptions.length = baroptions.length or 0.75*(x2-x1)
        baroptions.dir = 1
        local xdep = x1+( (x2-x1) - baroptions.length )/2
        baroptions.width = baroptions.width or -0.5 
        baroptions.width = -math.abs( baroptions.width )
        baroptions.labelpos = baroptions.labelpos or "N"
        self:Dcolorbar(Z(xdep,y2+bardist-baroptions.width),pal, baroptions)
    end
end

function graph:Dshadedregion(apath,pal,options)
    -- options are the Dshadedrectangle options
    -- apath is a path
    local x1,x2,y1,y2 = getbounds(path(apath))
    self:Beginclip( apath )
    self:Dshadedrectangle(x1,x2,y1,y2,pal,options)
    self:Endclip()
end
