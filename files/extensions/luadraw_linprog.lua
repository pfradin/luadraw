-- luadraw_linprog.lua 
-- date 2026/09/05
-- version 3.5
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   https://www.ctan.org/license/lppl

-- 2D and 3D linear programming in the current window

local ld = luadraw
local cpx = ld.cpx
local Z = cpx.Z
local graph = ld.graph
require 'luadraw_decorations'

local function str2function(expr,var)
-- returns the function defined by the string expr
-- var = {"x", "y",..}
    var = var or {"x", "y", "z"}
    return assert(load(
        "return function("..table.concat(var,",")..") return " .. expr .. " end",
        "expression",
        "t" -- text only
    ))()
end


------------------------------- 2D -------------------------------------
function ld.constraint2d(expr, inverse) 
-- expr = "a*x+b*y<c" or "a*x+b*y>c"
-- inverse = true/false (nverse orientation or not)
-- returns a line
    local pos = string.find(expr,"<")
    local sg
    if inverse then sg = ">" else sg = "<" end
    if pos == nil then 
        pos = string.find(expr,">")
        if pos == nil then return end
        if inverse then sg = "<" else sg = ">" end
    end
    local pre = string.sub(expr,1,pos-1)
    local post = string.sub(expr,pos+1,#expr)
    local f = str2function(pre.."-z*("..post..")")
    local a, b, c = f(1,0,0), f(0,1,0), f(0,0,1)
    return ld.lineEq(a,b,c,sg)
end

function ld.linprogSolve(constraints,x1,x2,y1,y2,objectives)
    objectives = objectives or {}
    local box = {Z(x1,y1),Z(x2,y1),Z(x2,y2),Z(x1,y2)}
    for _, aconstraint in ipairs(constraints) do
        local D = ld.constraint2d(aconstraint)
        box = ld.cutpolyline(box,D,true)
    end
    if (box ~= nil) and (#box > 0) then box = box[1] 
    else return end
    local rep = {}
    for _,obj in ipairs(objectives) do -- obj = "a*x+b*y"
        local f = str2function(obj,{"x","y"})
        local a, b = f(1,0), f(0,1)
        local D, n = ld.lineEq(a,b,0), Z(a,b)
        local dotMin, dotMax
        local Min, Max = math.huge, -math.huge
        for _,z in ipairs(box) do
            local m = cpx.dot(z,n)
            if m < Min then Min = m; dotMin = z end
            if m > Max then Max = m; dotMax = z end
        end
        local aux = {}
        aux.expr = obj
        aux.min = Min; aux.mindot = dotMin; aux.minline = ld.lineEq(a,b,-Min,">")
        aux.max = Max; aux.maxdot = dotMax; aux.maxline = ld.lineEq(a,b,-Max,">")
        table.insert(rep, aux)
    end
    return box, rep
end

function graph:DlinprogHalfPlanes(...)
-- g:DlinprogHalfPlanes(constraint1, options1, constraint2, options2, etc)
-- constraint = string as 'a*x+b*y<c' or 'a*x+b*y>c'
-- options = {pattern="", opacity=0.3, color=<default>, width=<default>, style=<default>,
--      label=, pos=, dir=, dist=, node_options= } ( decorations options )
    local constaint, options = "", {}
    local pattern, fill, label, color, width, style = "none", "", "", self.param.linecolor, self.param.linewidth, self.param.linestyle 
    local box = self:Box2d()
    local poly = self:Box2d()
    local n = select("#", ...)  -- number of arguments
    local sol = (n%2 == 1) --solution needed in, last argument must be a table (variable)
    local function aconstraint()
        local D = ld.constraint2d(constraint, true) -- inverse orientation
        pattern = options.pattern or pattern
        color = options.color or color
        width = options.width or width
        style = options.style or style
        opacity = options.opacity or 0.3
        options.dir = options.dir or D[2]
        options.pos = options.pos or "N"
        if math.abs(cpx.arg(options.dir)) > math.pi/2 then 
            options.dir = -options.dir
            options.pos = "S" 
        end
        options.anchor1d = options.anchor1d or 0.5
        options.node_options = options.node_options or "text="..color..",fill=white,fill opacity=0.8"
        options.dist = options.dist or 0.05
        local pos = string.find(constraint,"<")
        if pos == nil then 
            pos = string.find(constraint,">")
        end
        if pos ~= nil then
            local pre = string.sub(constraint,1,pos-1)
            local post = string.sub(constraint,pos+1,#constraint)
            label = "$"..pre.."="..post.."$"  -- automatic label
            label = string.gsub(label,"*","")
        else label = ""
        end
        options.label = options.label or label
        local P = ld.cutpolyline(box,D,true)
        if sol then
            local P1, P2 = ld.cutpolyline(poly,D,true)
            poly = P2
        end
        if pattern == "fill" then 
            self:Dpolyline(P,true, 'draw=none, fill='..color..', fill opacity='..opacity)
        elseif pattern ~= "none" then
            self:Dpolyline(P,true, 'draw=none, pattern='..pattern..', pattern color='..color..'!50')
        end
        options.draw_options = color..","..style..",line width=".. width/10
        self:Dline(D,options)
    end

    for k, arg in ipairs{...} do
        if k%2 == 1 then
            if k == n then 
                table.append(arg,poly[1]) -- polygon solution in the last arg
            else constraint = arg
            end
        else 
            options = arg
            aconstraint() 
        end
    end
end

function graph:DlinprogRegion(constraints, options, objectives, objectives_options)
-- constraints = list of strings as 'a*x+b*y<c' or 'a*x+b*y>c'
-- objectives = list of strings as 'c*x+d*y'
-- objectives_options = options for objectives
    objectives_options = objectives_options or {}
    options = options or {}
    local lines = options.lines or false -- to draw the lines or not
    local color = options.color or self.param.linecolor -- for the lines
    local width = options.width or self.param.linewidth -- for the lines
    local style = options.style or  self.param.linestyle -- for the lines
    local dots = options.dots or false -- to draw or not the polygon vertices
    local draw_options = options.draw_options or ""  --options for the solution
    local mark_options = options.mark_options or "" -- options for the dots
    local outside = options.outside or false -- paint outside of the region or not
    local view = options.view or self.param.viewport
    local out = options.out
    local x1, x2, y1, y2 = table.unpack(view)
    local region, sol = ld.linprogSolve(constraints,x1,x2,y1,y2,objectives)
    if outside then
        self:Beginclip( ld.polyline2path(region), true)
            self:Dpolyline(self:Box2d(),true,options)
        self:Endclip()
    else
        self:Dpolyline(region,true,options)
    end
    if lines then 
        for _, expr in ipairs(constraints) do 
            local D = ld.constraint2d(expr, true)
            local label = string.gsub(expr,"*","")
            label = string.gsub(label,"<","=")
            label = string.gsub(label,">","=")
            local opt = {}
            opt.draw_options = 'line width='..(width/10)..',color='..color..','..style
            opt.label = "$"..label.."$"
            opt.dir = D[2]
            opt.pos = "N"
            if math.abs(cpx.arg(opt.dir)) > math.pi/2 then 
                opt.dir = -opt.dir
                opt.pos = "S" 
            end
            opt.anchor1d =  0.5
            opt.node_options = "text="..color..",fill=white,fill opacity=0.8"
            opt.dist =  0.05            
            self:Dline(D,opt) end
    end
    if dots then self:Ddots(region, mark_options) end
    if out ~= nil then table.append(out,{region,sol}) end
    if objectives ~= nil then
        for _, obj in ipairs(sol) do
            self:DlinprogObjectiveLine(obj,objectives_options)
        end
    end
end


function graph:DlinprogObjectiveLine(obj,options)
-- obj={ expr = "a*x+b*y", min=, max=, mindot=, maxdot, minline=, maxline= }
-- options = { sense="min"/"max"/"minmax", draw_options="", 
--    label="", dir=, dist=, anchor1d=, node_options="" } (decorations options)
    options = options or {}
    local color = options.color or self.param.linecolor
    local width = options.width or self.param.linewidth
    local style = options.style or  self.param.linestyle 
    local sense = options.sense or "minmax"
    local mark_options = options.mark_options or ""
    options.node_options = options.node_options or "text="..color..",fill=white,fill opacity=0.8"
    local dir = options.dir
    local pos = options.pos
    local anchor = options.anchor
    local labelorig, label = options.label    
    local dots = options.dots or false
    options.dist = options.dist or 0.05
    if options.label == nil then
        label = string.gsub(obj.expr,"*","")
    else label = labelorig
    end
    options.draw_options = color..","..style..",line width=".. width/10
    if (sense == "min") or (sense == "minmax") then
        if options.label == nil then
            options.label = "$"..label.."="..ld.strReal(obj.min).."$"
        end
        local D1 = obj.minline
        options.dir = dir or D1[2]
        options.pos = pos or "S"
        if options.anchor1d == nil then
            options.anchor = anchor or obj.mindot
        else
            options.anchor = nil
        end
        self:Dline(D1, options)
        if dots then self:Ddots(obj.mindot,mark_options) end
        options.dir = dir; options.pos = pos; options.anchor = anchor
        options.label = labelorig
    end
    if (sense == "max") or (sense == "minmax") then
        if options.label == nil then
            options.label = "$"..label.."="..ld.strReal(obj.max).."$"
        end
        local D1 = obj.maxline
        options.dir = dir or D1[2]
        options.pos = pos or "N"
        if options.anchor1d == nil then
            options.anchor = anchor or obj.maxdot
        else
            options.anchor = nil
        end
        self:Dline(D1, options)
        if dots then self:Ddots(obj.maxdot,mark_options) end
        options.dir = dir; options.pos = pos; options.anchor = anchor
        options.label =labelorig
    end
    
end

------------------------------- 3D -------------------------------------
if ld.graph3d ~= nil then
    local pt3d = ld.pt3d
    local Origin, vecI, vecJ, vecK, M = pt3d.Origin, pt3d.vecI, pt3d.vecJ, pt3d.vecK, pt3d.M
    local graph3d = ld.graph3d
    
    function ld.constraint3d(expr, inverse) 
    -- expr = "a*x+b*y+c*z<d" or "a*x+b*y+c*z>d"
    -- inverse = true/false (nverse orientation or not)
    -- returns a line
        local pos = string.find(expr,"<")
        local sg
        if inverse then sg = ">" else sg = "<" end
        if pos == nil then 
            pos = string.find(expr,">")
            if pos == nil then return end
            if inverse then sg = "<" else sg = ">" end
        end
        local pre = string.sub(expr,1,pos-1)
        local post = string.sub(expr,pos+1,#expr)
        local f = str2function(pre.."-t*("..post..")", {"x","y","z","t"})
        local a, b, c , d= f(1,0,0,0), f(0,1,0,0), f(0,0,1,0), f(0,0,0,1)
        return ld.planeEq(a,b,c,d,sg)
    end
    
    function ld.linprogSolve3d(constraints,x1,x2,y1,y2,z1,z2,objectives)
    -- constraints =  list of "a*x+b*y+c*z<d" or "a*x+b*y+c*z>d"
    -- objectives = list of string as "u*x+v*y+w*z"
        objectives = objectives or {}  
        local box = ld.parallelep( M(x1,y1,z1), (x2-x1)*vecI, (y2-y1)*vecJ, (z2-z1)*vecK )
        for _, aconstraint in ipairs(constraints) do
            local P = ld.constraint3d(aconstraint)
            box = ld.cutpoly(box,P,true)
        end
        if (box == nil) then return end
        local rep = {}
        for _,obj in ipairs(objectives) do -- obj = "a*x+b*y+c*z"
            local f = str2function(obj,{"x","y","z"})
            local a, b, c = f(1,0,0), f(0,1,0), f(0,0,1)
            local P, n = ld.planeEq(a,b,c,0), M(a,b,c)
            local dotMin, dotMax
            local Min, Max = math.huge, -math.huge
            for _,A in ipairs(box.vertices) do
                local m = pt3d.dot(A,n)
                if m < Min then Min = m; dotMin = A end
                if m > Max then Max = m; dotMax = A end
            end
            local aux = {}
            aux.expr = obj
            aux.min = Min; aux.mindot = dotMin; aux.minplane = ld.planeEq(a,b,c,-Min,">")
            aux.max = Max; aux.maxdot = dotMax; aux.maxplane = ld.planeEq(a,b,c,-Max,">")
            table.insert(rep, aux)
        end
        return box, rep
    end    
end
