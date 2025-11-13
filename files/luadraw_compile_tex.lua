-- luadraw_compile_tex.lua
-- date 2025/11/13
-- version 2.3
-- Copyright 2025 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.

-- ce module permet de compiler un texte en tex, convertir le fichier en flattened postscript,
-- de lire le contenu de ce fichier sous forme d'une liste de chemins (avec épaisseur en tête de chaque chemin, et instruction de remplissage à la fin), cette liste L peut être affichée avec la méthode g:Dcompiled_tex(anchor,L, options)
-- ces chemins peuvent aussi être traduits en chemins 3d (path3d) dans un plan donné et être affichés avec la méthode Dcompiled_tex3d(L, options), ou bien traduits en lignes polygonales 3d (on perd alors l'épaisseur et la commande de remplissage).


preamble = "\\documentclass[12pt]{article}\n"
usepackage = "\\usepackage{amsmath,amssymb}\n\\usepackage{fouriernc}\n"

function compile_tex(text,out)
-- out est le nom du fichier crée, il ne doit comporter ni chemin, ni extension, il sera créé dans le dossier de travail de luadraw (nommé cachedir)
    local name = "tex2FlatPs"
    out = out or name
    local f = io.open(name..".tex","w")
    if f ~= nil then
        f:write(preamble)
        f:write(usepackage)
        f:write("\\pagestyle{empty}\n")
        f:write("\\begin{document}\n")
        f:write(text.."\n")
        f:write("\\end{document}\n")
        f:close()
        os.execute("lualatex -interaction=nonstopmode "..name..".tex "..name..".pdf" )
        os.execute("pdf2ps "..name..".pdf")
        os.execute("pstoedit -dt -pta -f ps -psarg  -r2400 "..name..".ps "..out..".eps")
        os.remove(name..".tex")
        os.remove(name..".pdf")
        os.remove(name..".ps")
        os.remove(name..".log")
        os.rename(out..".eps", cachedir..out..".eps")
        os.remove(name..".eps")
        os.remove(name..".aux")
    end
    return read_compiled_tex(out)
end

local tostrip = function(L, wd)
-- renvoie une bande centrée sur la ligne, d'une largeur égale à wd cm
    local ep = wd/2
    local i = cpx.I
    local a, b, c, v, u, w 
    local bord, dessus, ret = {}, {}, {}
    a = L[1]; b = L[2]
    while b == a do table.remove(L,1); b = L[2] end
    table.remove(L,1); table.remove(L,1)
    v = i*cpx.normalize(b-a)
    bord = {a-ep*v, a+ep*v}; dessus = {bord[2]}
    table.insert(ret,bord[1])
    c = b; b = a; v = v/i
    for _,z in ipairs(L) do
        a = b; b = c; c = z; v = cpx.normalize(c-b)
        if v == nil then 
            c = b; b = a; v =-u
        else
            w = cpx.normalize(u+v)
            if w == nil then
                bord = {b+ep*i*u, b-ep*i*u}
            else
                bord = projO( bord,{b,w},u)
            end
        end
        table.insert(ret,bord[1]); table.insert(dessus,1,bord[2])
    end
    return concat(ret,{c-ep*v*i, c+ep*v*i}, dessus)
end
 
function read_compiled_tex(file)
-- file est le nom (sans chemin sans extension) d'un fichier eps
-- créé par la fonction compile_tex() dans le dossier de travail de luadraw (nommé cachedir)
-- la fonction lit le contenu du fichier et renvoie une liste de chemins commençant par l'épaisseur de ligne et finissant la commande de remplissage.
    file = file or "tex2FlatPs"
    file = cachedir..file..".eps"
    local f = io.open(file,"r")
    if f == nil then print('no file '..file..' found'); return end
    local str, lwd -- string and linewidth
    local lines = {}
    local ret = {}
    local x1,x2,y1,y2 = math.huge, -math.huge, math.huge, -math.huge
    local instruction = {["curveto"]="b", ["lineto"]="l",["moveto"]="m", ["closepath"]="cl"}

    local find_path = function()
        local find = false
        while (#lines ~= 0) and (not find) do
            str = table.remove(lines)
            if string.find(str,"setlinewidth") then
                lwd = string.match(str,"%d*%.%d*") -- pt
                lwd = tostring(tonumber(lwd)*10) --tostring(1+math.floor(tonumber(lwd)*10))
            end
            find = (string.find(str,"newpath") ~= nil)
        end
        if find then
            str = table.remove(lines) -- next line
        end
        return find
    end
    
    local read_path = function()
        local finish = false
        local cp = {lwd} -- current path begins with linewidth
        local nb = 1
        local u, x, y, inst
        while not finish do
            for w in string.gmatch(str, "[%d%a]*[%.%d%a][%d%a]*") do
                u = tonumber(w)
                if u ~= nil then -- u is a number
                    if nb == 1 then x = u; nb = 2  
                    else 
                        y = u; table.insert(cp, Z(x,y)); nb = 1
                        if x < x1 then x1 = x end; if x > x2 then x2 = x end
                        if y < y1 then y1 = y end; if y > y2 then y2 = y end
                    end
                else -- w is a string
                    inst = instruction[w]
                    if inst ~= nil then table.insert(cp,inst) end
                    finish = (w =='eofill') or (w =='stroke') or (w =='fill') or (w =='clip')
                    if finish then table.insert(cp,w) end
                end
            end
            if not finish then
                str = table.remove(lines) -- next line
                finish = (#lines == 0)
            end
        end
        table.insert(ret,cp)
    end

    for line in io.lines(file) do
        table.insert(lines,1,line)
    end
    while find_path() do
        read_path()
    end
    local c = Z(x1+x2,y1+y2)/2 --center
    local ratio = 2.54/72
    local mat = matrixof( function(z) return (z-c)*ratio end )
    local rep =  {}
    rep.path = {}
    ret = mtransform(ret,mat)
    for _, cp in ipairs(ret) do --we change line to strip
        local ends = cp[#cp]
        if ends == "stroke" then -- we have a line, we transform it into a strip
            lwd = table.remove(cp,1)
            local char = table.remove(cp,2) -- remove "m"
            table.remove(cp); table.remove(cp) -- remove "l" and "stroke"
            local ep = tonumber(lwd)/10*pt
            cp = tostrip(cp,ep)
            table.insert(cp,2,char)
            table.insert(cp,1,lwd)
            table.insert(cp,"l"); table.insert(cp,"cl")
            table.insert(cp,"fill")
        end
        table.insert(rep.path,cp)
    end
    rep.bb =  table.pack(getbounds(mtransform({ Z(x1,y1),Z(x2,y2)},mat) ))
    return rep
end

local splitLongSeg = function(L) -- to divide segments that are too long
    -- L is a list of list of complex numbers
    local ret, cp, B, A = {}
    for _, lg in ipairs(L) do
        B = lg[1]; cp = {}
        for k = 2, #lg do
            A = B; B = lg[k]
            local len = cpx.abs(B-A)
            local nb = math.floor(10*len)
            insert(cp, linspace(A,B,nb+2) )
        end
        table.insert(ret,cp)
    end
    return ret
end

function compiled_tex2polyline(L,scale)
-- L est le résultat de compile_tex()
    if L == nil then return end
    scale = scale or 1
    local scx, scy, P = 1, 1, L.path
    if scale ~= 1 then
        if type(scale) == "number" then scx = scale; scy = scale 
        else scx, scy = table.unpack(scale)
        end
        local c = isobar(L.bb)
        local mat = {c+Z(c.re*(1-scx),c.im*(1-scy)),Z(scx,0),scy*cpx.I} 
        P = mtransform(P, mat)
    end
    local ret = {}
    for _, cp in ipairs(P) do
        local p = table.copy(cp) -- L must not be modified
        local lwd = table.remove(p,1)
        local cmd = table.remove(p) -- We remove the beginning and the end, leaving only the path
        local C1 = splitLongSeg( path(p) )
        insert(ret,C1)
    end
    return ret -- list of list of complex numbers
end

function graph:Dcompiled_tex(anchor,L, options)
--L est le résultat de la fonction read_compiled_tex(filename)
--options : {scale=1, color= default, dir=nil, hollow=false, drawbox = false, draw_options=""}
-- scale = scale number or table {scalex, scaley}
-- dir = {vector1, vector2}
-- drawbox= false
-- draw_options=""
    anchor = anchor or 0
    anchor = toComplex(anchor)
    options = options or {}
    options.color = options.color or self.param.linecolor
    options.scale = options.scale or 1
    options.drawbox = options.drawbox or false
    options.hollow = options.hollow or false
    options.draw_options= options.draw_options or ""    
    if L == nil then return end
    local lwd, cmd, scx, scy
    
    local oldfillstyle = self.param.fillstyle
    local oldfillcolor = self.param.fillcolor
    local oldlinestyle = self.param.linestyle
    local oldlinecolor = self.param.linecolor
    local oldlinewidth = self.param.linewidth
    
    if type(options.scale) == "number" then 
        scx = options.scale; scy = options.scale
    else
        scx, scy = table.unpack(options.scale)
    end
    local x1,x2,y1,y2 = table.unpack(L.bb)
    local c = Z(x1+x2,y1+y2)/2
    local mat = {anchor+Z(c.re*(1-scx),c.im*(1-scy)),Z(scx,0),scy*cpx.I} 
    if options.dir ~= nil then
        local u, v = table.unpack(options.dir)
        u = u/cpx.abs(u); v = v/cpx.abs(v)
        mat = composematrix(mat,{0,u,v})
    end
    local C = mtransform(L.path,mat)
    local bb = mtransform( {Z(x1,y1),Z(x2,y1),Z(x2,y2),Z(x1,y2)}, mat) -- boite englobante
     for _, p in ipairs(C) do
        lwd = table.remove(p,1)
        cmd = table.remove(p)
        if (cmd == "eofill") or (cmd == "fill") then
            if options.hollow then
                self:Lineoptions("solid",options.color,oldlinewidth); self:Filloptions("none")
            else
                self:Lineoptions("noline"); self:Filloptions("full",options.color,1,(cmd == "eofill") )
            end
            self:Dpath(p,options.draw_options)
        elseif cmd == "stroke" then
            self:Lineoptions("solid",options.color,lwd*scx); self:Filloptions("none")
            self:Dpath(p,options.draw_options)
        end
    end
    self:Filloptions(oldfillstyle,oldfillcolor)
    self:Lineoptions(oldlinestyle,oldlinecolor,oldlinewidth); 
    if options.drawbox then
        self:Dpolyline(bb,true)
    end
end

-----------------------------------------------------
--- partie pour la 3d uniquement --------------------
-----------------------------------------------------

local splitLongSeg3d = function(L) -- to divide segments that are too long
    -- L is a list of list of 3d points
    local ret, cp, B, A = {}
    for _, lg in ipairs(L) do
        B = lg[1]; cp = {}
        for k = 2, #lg do
            A = B; B = lg[k]
            local len = pt3d.abs(B-A)
            local nb = math.floor(10*len)
            insert(cp, linspace(A,B,nb+2) )
        end
        table.insert(ret,cp)
    end
    return ret
end


function graph:Compiled_tex2path3d(L,options)
-- L est le résultat de la fonction read_compiled_tex(filename)
-- la fonction convertit les chemins en chemins 3d et renvoie le résultat, celui-ci peut être dessiné avec la méthode g:Dcompiled_tex3d() 
-- options = {anchor=Origin, scale = 1, dir = {vecJ,vecK}, polyline=false}
    options= options or {}
    options.scale= options.scale or 1
    options.anchor= options.anchor or Origin
    options.polyline= options.polyline or false
    options.dir= options.dir or {vecJ,vecK}
    local plane = {options.anchor, pt3d.prod(options.dir[1], options.dir[2])}
    local scx, scy, x1,x2,y1,y2
    if L == nil  then return end
    if type(options.scale) == "number" then 
        scx = options.scale; scy = options.scale
    else
        scx, scy = table.unpack(options.scale)
    end
    x1,x2,y1,y2 = table.unpack(L.bb)
    local c = Z((x1+x2)/2,(y1+y2)/2)
    local A = options.anchor
    local u = pt3d.normalize(options.dir[1])
    local v = pt3d.normalize(options.dir[2])
    local mat = {-c,Z(1,0),cpx.I} 
    local C = mtransform(L.path,mat)
    local f = function(z)
        if type(z)=="string" then return z else return A+scx*z.re*u+scy*z.im*v end
    end
    local ret = {}
    if not options.polyline then
        ret.bb = ftransform({Z(x1,y1),Z(x2,y1),Z(x2,y2),Z(x1,y2),"l","cl"},f) -- boite englobante
        for _,p in ipairs(C) do
            local lwd = tonumber(p[1])*pt
            lwd = self:Abs(self:Proj3dV(lwd*v))*mm
            p[1] = tostring( lwd*scx ) -- line width adaptation
        end
        ret.path = ftransform(C,f)
        return ret
    else
        for _,p in ipairs(C) do
            local lwd = table.remove(p,1)
            local cmd = table.remove(p) -- We remove the beginning and the end, leaving only the path
            local C1 = splitLongSeg3d( path3d( ftransform(p,f) ) )
            insert(ret,C1)
        end
        return ret -- list of list of 3d points
    end
end

function graph:Dcompiled_tex3d(L, options)
--L est le résultat de la méthode g:Conv_compiled_tex3d(filename,options)
--options : {color= default, hollow=false, drawbox=false, draw_options=""}
    options = options or {}
    options.draw_options= options.draw_options or ""
    options.color = options.color or self.param.linecolor
    options.drawbox = options.drawbox or false
    options.hollow = options.hollow or false
    local lwd, cmd
    if L == nil then return end
    local oldfillstyle = self.param.fillstyle
    local oldfillcolor = self.param.fillcolor
    local oldlinestyle = self.param.linestyle
    local oldlinecolor = self.param.linecolor
    local oldlinewidth = self.param.linewidth
    for _, p in ipairs(L.path) do
        lwd = table.remove(p,1)
        cmd = table.remove(p)
        if (cmd == "eofill") or (cmd == "fill") then
            if options.hollow then
                self:Lineoptions("solid",options.color,oldlinewidth); self:Filloptions("none")
            else
                self:Lineoptions("noline"); self:Filloptions("full",options.color,1,(cmd == "eofill") )
            end
            self:Dpath3d(p,options.draw_options)
        elseif cmd == "stroke" then
            self:Lineoptions("solid",options.color,lwd); self:Filloptions("none")
            self:Dpath3d(p,options.draw_options)
        end
    end
    self:Filloptions(oldfillstyle,oldfillcolor)
    self:Lineoptions(oldlinestyle,oldlinecolor,oldlinewidth); 
    if options.drawbox then
        self:Dpath3d(L.bb)
    end
end
