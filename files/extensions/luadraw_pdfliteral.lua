-- luadraw_pdfliteral.lua
-- date 2026/09/05
-- version 3.5
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   https://www.ctan.org/license/lppl

--To write the following to the PDF stream:
--a large number of 2D paths or polylines
--a cloud of numerous points
--a large list of 3D facets.

local ld = luadraw
local cpx = ld.cpx

function ld.graph:bpCoord(z) -- coordinates, conversion cm -> bp
    local a, b = self:coord(z.re,z.im)
    local c = 28.3464567
    return  ld.strReal(a*c,2).." "..ld.strReal(b*c,2) -- two decimals
end

function ld.graph:Dliteralpath(...)
-- path1, args1, path2, args2, ...
-- args = {fill=rgb table, draw=rgb table}, fill="none" => no fill, draw="none" => no line (fill is solid)
    local fill, draw, width, ends = {}, {}, {}
    local literalstr = {}
    local strcolor, strfill, strdraw, strwidth 
    local idmatrix = ld.isID(self.matrix)
    local oldlinewidth = self.param.linewidth
    local oldlinecolor = self.param.linecolor 
    local oldfillcolor = self.param.fillcolor 
    
    local function apath(path,args)
        fill = args.fill or fill
        draw = args.draw or draw
        width = args.width or width
        if (draw == "none") and (fill == "none") then return -- no fill, no stroke
        elseif (draw == "none") then ends = "f" -- only fill
        elseif (fill == "none") then ends = "s" -- only stroke
        else ends = "b" -- fill and stroke
        end 
        if (type(fill) == "table") and (#fill == 3) then
            strcolor = ld.strReal(fill[1],3).." "..ld.strReal(fill[2],3).." "..ld.strReal(fill[3],3)
            if strcolor ~= strfill then
                table.insert(literalstr,strcolor.." rg" ) -- fill color
                strfill = strcolor
            end
        end
        if (type(draw) == "table") and (#draw == 3) then
            strcolor = ld.strReal(draw[1],3).." "..ld.strReal(draw[2],3).." "..ld.strReal(draw[3],3)
            if strcolor ~= strdraw then
                table.insert(literalstr,strcolor.." RG" ) -- line color
                strdraw = strcolor
            end
        end
        if (type(width) == "number") then
            strcolor = ld.strReal(width/10*ld.pt*28.3464567,3)
            if strcolor ~= strwidth then
                table.insert(literalstr,strcolor.." w" ) -- line width
                strwidth = strcolor
            end
        end           
        local aux = {}
        if idmatrix then path = self:Mtransform( ld.convpath(path) ) -- only "m", "l", "b" and "cl"
        else path = ld.convpath(path)
        end
        local last
        for _,z in ipairs(path) do -- only "m", "l", "b" and "cl"
            if z == "m" then
                table.insert(literalstr, self:bpCoord(aux[#aux]).." m")
                aux = {}
            elseif z == "l" then
                for _,c in ipairs(aux) do
                    local bpcoord = self:bpCoord(c)
                    if  last ~= bpcoord then table.insert(literalstr, bpcoord.." l") end
                    last = bpcoord
                end
                aux = {}
            elseif z == "b" then
                local c1, c2, c3 = table.unpack(aux)
                table.insert(literalstr, self:bpCoord(c1).." "..self:bpCoord(c2).." "..self:bpCoord(c3).." c")
                aux = {}
            elseif z == "cl" then
                table.insert(literalstr, "h")
            else table.insert(aux,z)
            end            
        end
        table.insert(literalstr, ends)
    end
    
    local n = select("#", ...)  -- Nombre total d'arguments
    for i = 1, n-1, 2 do   -- Pas de 3 (1,4,7...)
        local path, args = select(i, ...)  -- Récupère les 2 args
        if path ~= nil then apath(path,args) end
    end
    self:Writeln("\\luadrawLiteral{"..table.concat(literalstr,"\n").."}")
    self.param.linewidth = nil; self.param.linecolor = nil; self.param.fillcolor = nil
    self:Linewidth(oldlinewidth); self:Linecolor(oldlinecolor); self:Filloptions(nil,oldfillcolor)

end

function ld.graph:Dliteralpolyline(...)
-- polyline1, args1, polyline2, args2, ...
-- args = {fill= rgb table, draw=rgb table, close = false}, fill="none" => no fill, draw="none" => no line (fill is solid)
    local fill, draw, width, ends = {}, {}, {}
    local close = false
    local literalstr = {}
    local strcolor, strfill, strdraw, strwidth
    local idmatrix = ld.isID(self.matrix)
    local oldlinewidth = self.param.linewidth
    local oldlinecolor = self.param.linecolor 
    
    local function apolyline(polyline,args)
        if (type(polyline[1]) == "number") or cpx.isComplex(polyline[1]) then
            polyline = {polyline}
        end
        fill = args.fill or fill
        draw = args.draw or draw
        width = args.width or width
        if args.close ~= nil then close = args.close end
        if (draw == "none") and (fill == "none") then return -- no fill, no stroke
        elseif (draw == "none") then ends = "f" -- only fill
        elseif (fill == "none") then ends = "s" -- only stroke
        else ends = "b" -- fill and stroke
        end 
        if (type(fill) == "table") and (#fill == 3) then
            strcolor = ld.strReal(fill[1],3).." "..ld.strReal(fill[2],3).." "..ld.strReal(fill[3],3)
            if strcolor ~= strfill then
                table.insert(literalstr,strcolor.." rg" ) -- fill color
                strfill = strcolor
            end
        end
        if (type(draw) == "table") and (#draw == 3) then
            strcolor = ld.strReal(draw[1],3).." "..ld.strReal(draw[2],3).." "..ld.strReal(draw[3],3)
            if strcolor ~= strdraw then
                table.insert(literalstr,strcolor.." RG" ) -- line color
                strdraw = strcolor
            end
        end
        if (type(width) == "number") then
            strcolor = ld.strReal(width/10*ld.pt*28.3464567,3)
            if strcolor ~= strwidth then
                table.insert(literalstr,strcolor.." w" ) -- line width
                strwidth = strcolor
            end
        end              
        local aux = {}
        if idmatrix then polyline = self:Mtransform( polyline ) end
        local last
        for _,cp in ipairs(polyline) do
            for k,z in ipairs(cp) do 
                z = cpx.toComplex(z)
                if k == 1 then table.insert(literalstr, self:bpCoord(z).." m") 
                else
                    local bpcoord = self:bpCoord(z)
                    if  last ~= bpcoord then table.insert(literalstr, bpcoord.." l") end
                    last = bpcoord
                end
            end
            if close then table.insert(literalstr, "h") end
            table.insert(literalstr, ends)
        end
    end
    
    local n = select("#", ...)  -- Nombre total d'arguments
    for i = 1, n-1, 2 do   -- Pas de 3 (1,4,7...)
        local polyline, args = select(i, ...)  -- Récupère les 3 args
        if polyline ~= nil then apolyline(polyline,args) end
    end
    self:Writeln("\\luadrawLiteral{"..table.concat(literalstr,"\n").."}")
    self.param.linewidth = nil; self.param.linecolor = nil
    self:Linewidth(oldlinewidth); self:Linecolor(oldlinecolor)
end

function ld.graph:Dliteraldots(...)
-- {dots1}, args1, {dots2}, args2, ...
-- args = {color=rgb table, width=line width}
    local draw, width = {}, {}
    local close = false
    local literalstr = {}
    local strcolor, strdraw, strwidth
    local idmatrix = ld.isID(self.matrix) 
    local oldlinestyle = self.param.linestyle
    local oldlinewidth = self.param.linewidth
    local oldlinecolor = self.param.linecolor
    local oldlinecap = self.param.linecap
    self:Linecap("round"); self:Linestyle("solid")
    
    local function adots(dots,args)
        if (type(dots[1]) == "number") or cpx.isComplex(dots[1]) then
            dots = {dots}
        end
        draw = args.color or draw
        width = args.width or width
        if (draw == "none") then return  end -- no stroke
        if (type(draw) == "table") and (#draw == 3) then
            strcolor = ld.strReal(draw[1],3).." "..ld.strReal(draw[2],3).." "..ld.strReal(draw[3],3)
            if strcolor ~= strdraw then
                table.insert(literalstr,strcolor.." RG" ) -- line color
                strdraw = strcolor
            end
        end
        if (type(width) == "number") then
            strcolor = ld.strReal(width/10*ld.pt*28.3464567,3)
            if strcolor ~= strwidth then
                table.insert(literalstr,strcolor.." w" ) -- line width
                strwidth = strcolor
            end
        end        
        local aux = {}
        if idmatrix then dots = self:Mtransform( dots ) end
        local last
        for _,cp in ipairs(dots) do
            for k,z in ipairs(cp) do 
                z = cpx.toComplex(z)
                local bpcoord = self:bpCoord(z)
                table.insert(literalstr, bpcoord.." m") 
                table.insert(literalstr, bpcoord.." l")
            end
        end
        table.insert(literalstr, "s")
    end
    
    local n = select("#", ...)  -- Nombre total d'arguments
    for i = 1, n-1, 2 do   -- Pas de 2 (1,3,5...)
        local dots, args = select(i, ...)  -- Récupère les 2 args
        if dots ~= nil then adots(dots,args) end
    end
    self:Writeln("\\luadrawLiteral{"..table.concat(literalstr,"\n").."}")
    self.param.linewidth = nil; self.param.linecolor = nil
    self:Lineoptions(oldlinestyle,oldlinecolor,oldlinewidth)
    self:Linecap(oldlinecap)
end


if ld.graph3d ~= nil then
    local pt3d = ld.pt3d
    local M = pt3d.M
    
    function ld.graph3d:Dliteralfacet(...) --Dliteralfacet(F1,args1, F2,args2, ...)
    -- dessine une liste de facettes F (avec coordonnées 3d)
    -- args est une table à 11 champs :
    -- args = {mode=0/1/2/3/4/5, contrast=1, backcull=true/false,clip=false, edgecolor=default, edgewidth=defaut, twoside=true/false, color=ld.White, usepalette=nil}    
        --mode=0: arêtes seulement
        --mode=1 et 2: faces peintes couleur unie+arêtes visibles
        --mode=3 et 4 faces peintes couleur nuancée+arêtes visibles
        --mode=5 faces peintes couleur nuancée pas d'arêtes
        local mode = 3
        local contrast = 1
        local edgecolor = ld.Black
        local edgewidth = self.param.linewidth
        local backcull = false
        local twoside = true
        local reverse = false
        local clip = false
        local color = ld.White
        local usepalette = nil
        local opacity = self.param.fillopacity
        local S, face, args = {}
        local literalstr = {}
        local strcolor, strfill, strdraw, strwidth 
        for k,F in ipairs{...} do
            if k%2 == 1 then 
                face = F
                if face.vertices ~= nil then face = ld.poly2facet(face) end
            else
                args = F
                if args.mode == nil then args.mode = mode else mode = args.mode end
                if args.contrast == nil then args.contrast = contrast else contrast = args.contrast end
                if args.edgecolor == nil then args.edgecolor = edgecolor else edgecolor = args.edgecolor end
                if args.edgewidth == nil then args.edgewidth = edgewidth else edgewidth = args.edgewidth end
                if args.backcull == nil then args.backcull = backcull else backcull = args.backcull end
                if args.clip == nil then args.clip = clip else clip = args.clip end
                if args.color == nil then args.color = color else color = args.color end
                if args.twoside == nil then args.twoside = twoside else twoside = args.twoside end
                if args.reverse == nil then args.reverse = reverse else reverse = args.reverse end
                if reverse then face = ld.reverse_face_orientation(face) end
                if args.usepalette == nil then args.usepalette = usepalette else usepalette = args.usepalette end
                if pt3d.isPoint3d(face[1]) then face = {face} end
                if args.clip then face = ld.clip3d(face,self:Box3d()) end
                if not ld.isID3d(self.matrix3d) then face = self:Mtransform3d(face) end -- on applique la matrice de transformation
                if args.backcull then face = self:Sortfacet(face, true) end
                if args.usepalette == nil then
                    args.getcolor = function(f)
                        return args.color
                    end
                else
                    local pal, mode, minmax = table.unpack(args.usepalette)
                    args.getcolor = ld.define_getcolor(face,pal,mode,args.color,minmax)
                end            
                for _, f in ipairs(face) do
                    table.insert(f,args) -- chaque facette est accompagnée de ses arguments
                    table.insert(S,f)
                end
            end
        end
        local oldmatrix = self.matrix3d
        self.matrix3d = ld.ID3d
        S = self:Sortfacet(S)
        for _,F in ipairs(S) do
            args = table.remove(F)
            local ends = "b" -- fill and stroke
            local f1 = self:Mtransform(self:Proj3d(F)) -- conversion 3D -> 2D
            local color = args.color
            if (args.mode == ld.mWireframe) then -- arêtes seulement (mWireframe)
                if args.usepalette ~= nil then 
                    args.edgecolor = args.getcolor(F)
                end
            elseif (args.mode == ld.mFlat) or (args.mode == ld.mFlatHidden) then --faces unies
                color = args.getcolor(F)
            else --mode = 3,4 ou 5 faces peintes
                color = self:Adjust_color(F,args.getcolor(F),args.contrast,args.twoside,true)        
            end
            local strcolor = ld.strReal(color[1],3).." "..ld.strReal(color[2],3).." "..ld.strReal(color[3],3)
            if args.mode ~= ld.mWireframe then
                if strcolor ~= strfill then
                    table.insert(literalstr,strcolor.." rg" ) -- fill color
                    strfill = strcolor
                end
            end
            if (args.mode ~= ld.mShadedOnly) then
                local wd = ld.strReal(args.edgewidth/10*ld.pt*28.3464567,3)
                if wd ~= strwidth then
                    table.insert(literalstr,wd.." w" ) -- line width
                    strwidth = wd
                end
            end
            if (args.mode == ld.mShadedOnly) then
                if (opacity == 1) then
                    if strcolor ~= strdraw then
                        table.insert(literalstr,strcolor.." RG" ) -- line color same as fill color
                        strdraw = strcolor
                    end
                else ends = "f" --fill only
                end
            elseif args.mode == ld.mWireframe then 
                ends = "s" -- stroke only
            end
            if (ends ~= "f") and (args.mode ~= ld.mShadedOnly) then
                strcolor = ld.strReal(args.edgecolor[1],3).." "..ld.strReal(args.edgecolor[2],3).." "..ld.strReal(args.edgecolor[3],3)
                if strcolor ~= strdraw then
                    table.insert(literalstr,strcolor.." RG" ) -- line color
                    strdraw = strcolor
                end  
            end
            local n = #f1
            table.insert(literalstr, self:bpCoord(f1[1]).." m")
            for k = 2, n-1 do
                table.insert(literalstr, self:bpCoord(f1[k]).." l")
            end
            table.insert(literalstr, self:bpCoord(f1[n]).." l") -- facet inserted
            table.insert(literalstr, ends) -- facet inserted        
        end
        self:Writeln("\\luadrawLiteral{"..table.concat(literalstr,"\n").."}")
        self.matrix3d = oldmatrix
    end
    
end
