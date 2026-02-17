-- luadraw_povray.lua
-- date 2026/02/17
-- version 2.6
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.

-- This module adds a (very basic) export to POV-RAY

local oldtostring = tostring
local tostring = function(x)
    if type(x) == "number" then
        return strReal(x,6)
    else
        return x
    end
end
local Pov_preamble = {""}
local Pov_declarations = {""}
local Pov_renderings = {""}
local Pov_export = Pov_declarations
local Pov_object = 0
local Pov_cmd_win = "pvengine64.exe"
local Pov_cmd_unix = "povray"
local Pov_param = "-V +A +FN"
local Pov_param_ext = "/RENDER /EXIT"
local Pov_includefiles = {} --list of strings (inc files)
local Pov_size = ""
local Pov_bg = ""
local Pov_scale = 1
local Pov_shadow = true

function graph3d:Pov_clean()
    Pov_preamble = {""}
    Pov_declarations = {""}
    Pov_renderings = {""}
    Pov_export = Pov_declarations
    Pov_object = 0
end

function graph3d:Pov_new(options)
-- options = {bg="" ,imagescale= 1, shadow=true, param="-V +A +FN", pov_cmd=default, win_param_ext=default}
    options = options or {}
    Pov_scale = options.imagescale or 1
    Pov_bg = options.bg or ""
    Pov_shadow = options.shadow
    if Pov_shadow == nil then Pov_shadow = true end
    Pov_param = options.param or "-V +A +FN"
    if os.type == "windows" then
        Pov_cmd_win = options.pov_cmd or "pvengine64.exe"
    else
        Pov_cmd_unix = options.pov_cmd or "povray"
    end
    Pov_param_ext = options.win_param_ext or "/RENDER /EXIT"
    self:Pov_clean()
 end
 
function graph3d:Pov_include(...) -- "file1.inc", "file2.inc", ...
    for _, incfile in ipairs{...} do
        table.insert(Pov_includefiles, incfile)
    end
end

function graph3d:Pov_save(filename) -- filename without path, without extension
    if filename == nil then
        local n = #graph_name
        filename = string.sub(graph_name,1,n-3).."pov"
    else
        filename = cachedir..filename..".pov"
    end
    local f = io.open(filename,"w")
    if f == nil then print("Can't open file "..filename..". Abort...")
    else
        self:Pov_do_preamble()
        local str = concat(Pov_preamble, {"//declarations"},Pov_declarations,{"//renderings"},Pov_renderings)
        for _, lg in ipairs(str) do
            f:write(lg.."\n")
        end
        f:close()
    end
end

function graph3d:Pov_exec(filename) -- filename without path, without extension
    self:Pov_save(filename)
    if filename == nil then
        local n = #graph_name
        filename = string.sub(graph_name,1,n-3).."pov"
    else
        filename = cachedir..filename..".pov"
    end
    local param = Pov_param
    local cmd
    if Pov_bg == "" then 
        param = param .. " +UA" --transparent background
    end  
    if os.type == "windows" then
        cmd = Pov_cmd_win.." "..Pov_size..param.." "..filename.." "..Pov_param_ext
    else
        cmd = Pov_cmd_unix.." "..Pov_size..param.." "..filename
    end
    print("execute: "..cmd)
    os.execute(cmd)  -- this instruction needs a compilation with -shell-escape option
end

function graph3d:Pov_show(filename)
    if filename == nil then
        local n = #graph_name
        filename = string.sub(graph_name,1,n-3).."png"
    end
    self:Dimage(filename, Z(self.Xmin,self.Ymin), {pos="NE", graphics_options="width="..self:Graphwidth().."cm, height="..self:Graphheight().."cm"})
end


local get_num = function()
    Pov_object = Pov_object+1
    return Pov_object
end

local pov_write = function(str)
-- écrit la chaîne str sans retour à la ligne
    if str ~= "" then
        local n = #Pov_export
        Pov_export[n] = Pov_export[n]..str
    end
end

local pov_writeln = function(str)
--  écrit la chaîne str avec retour à la ligne
    pov_write(str)
    table.insert(Pov_export,"") -- on démarre une nouvelle ligne   
end

function graph3d:Pov_write(str)
    pov_write(str)
end

function graph3d:Pov_writeln(str)
    pov_writeln(str)
end

local strdot = function(A) 
    return "<"..tostring(-A.x)..","..tostring(A.y)..","..tostring(A.z)..">"
end

local strdot2 = function(A) -- without changing x-coordinate
    return "<"..tostring(A.x)..","..tostring(A.y)..","..tostring(A.z)..">"
end


local write_rendering = function(name,color,opacity,ambient,diffuse,phong,shadow,mytexture)
    Pov_export = Pov_renderings
    pov_writeln("\nobject{ "..name)
    if mytexture ~= nil then
        pov_writeln("  "..mytexture)
    else
        pov_writeln("  texture{ ")
        if type(color) == "string" then -- pigment can come from a file as textures.inc
            pov_writeln("         pigment{ "..color.."}") -- transmit "..tostring(1-opacity).."}")
        else
            pov_writeln("         pigment{ color rgbt<"..tostring(color[1])..","..tostring(color[2])..","..tostring(color[3])..","..tostring(1-opacity)..">}")
        end
        pov_writeln("         finish{ ambient "..tostring(ambient).." diffuse "..tostring(diffuse).." phong "..tostring(phong).."} }")
    end
    if not shadow then pov_writeln("  no_shadow}") 
    else
        pov_writeln("      }")
    end
    Pov_export = Pov_declarations
end

local write_matrix2 = function(matrix) -- with changing of x-coordinate
        local V4, V1, V2, V3 = matrix[1], matrix[2], matrix[3], matrix[4]
        pov_writeln("    matrix <"..tostring(-V1.x)..","..tostring(V1.y)..","..tostring(V1.z)..",")
        pov_writeln("            "..tostring(-V2.x)..","..tostring(V2.y)..","..tostring(V2.z)..",")
        pov_writeln("            "..tostring(-V3.x)..","..tostring(V3.y)..","..tostring(V3.z)..",")
        pov_writeln("            "..tostring(-V4.x)..","..tostring(V4.y)..","..tostring(V4.z)..">")
end

local write_matrix = function(matrix) -- without changing of x-coordinate
    if not isID3d(matrix) then
        local V4, V1, V2, V3 = matrix[1], matrix[2], matrix[3], matrix[4]
        pov_writeln("    matrix <"..tostring(V1.x)..","..tostring(-V1.y)..","..tostring(-V1.z)..",")
        pov_writeln("            "..tostring(-V2.x)..","..tostring(V2.y)..","..tostring(V2.z)..",")
        pov_writeln("            "..tostring(-V3.x)..","..tostring(V3.y)..","..tostring(V3.z)..",")
        pov_writeln("            "..tostring(-V4.x)..","..tostring(V4.y)..","..tostring(V4.z)..">")
    end
end

local write_clip = function(cliplist,box)
-- box = true or false, 
-- if true then data = {M(x1,y1,z1), M(x2,y2,z2)} is a box, else data = {A,n} is  a plane
    if cliplist == nil then return end
    if (type(cliplist) ~= "table") or isPoint3d(cliplist[1]) then cliplist = {cliplist} end
    for _, data in ipairs(cliplist) do
        if type(data) == "string" then -- data must the name of an existing object
            pov_writeln("    clipped_by{ object{ "..data.."} }")
        else
            local f = strdot
            local M1, M2 = table.unpack(data)
            if box == nil then box = true end
            if box then -- box or sphere
               if isPoint3d(M2) then 
                    pov_writeln("    clipped_by{ box{ ".. f(M1).." ".. f(M2).."} }")
                else
                 pov_writeln("    clipped_by{ sphere{ ".. f(M1)..", ".. tostring(M2).."} }")
                end 
            else -- plane
                local O = proj3d(Origin, data)
                M2 = pt3d.normalize(M2)
                local d = pt3d.abs(O)
                --M2 = -M2
                if pt3d.dot(O,M2) > 0 then d = -d end -- distance à l'origine
                pov_writeln("  clipped_by { plane{ "..f(-M2)..", "..tostring(d).." } }")
            end
        end
    end
end

--------------------- preamble -----------------

function graph3d:Pov_do_preamble()
    Pov_export = Pov_preamble
    local param = Pov_param
    if Pov_bg == "" then 
        param = param .. " +UA" --transparent background
    end
    local wd, ht, H, W = self:Graphwidth(), self:Graphheight()
    if ht > wd then 
        H = math.floor(640*Pov_scale); W = math.floor(640*wd/ht*Pov_scale)
    else 
        W = math.floor(640*Pov_scale); H = math.floor(640*ht/wd*Pov_scale)
    end
    local rep = {}
    Pov_size = " +W"..tostring(W).." +H"..tostring(H).." "
    pov_writeln("//luadraw "..luadraw_version)
    pov_writeln("//options:"..Pov_size..param)
    pov_writeln("#version 3.7;")
    for _,incfile in ipairs(Pov_includefiles) do
        pov_writeln('#include "'..incfile..'"')
    end
    pov_writeln("global_settings { charset utf8\n     ambient_light rgb 1.5\n     assumed_gamma 1.8}")
    if Pov_bg == "" then 
        pov_writeln("background{ color srgbt<0,0,0,1> }")
    elseif type(Pov_bg) == "string" then
        pov_writeln("background{ "..Pov_bg.." }")
    else
        local r,g,b = table.unpack(Pov_bg)
        pov_writeln("background{ color rgb<"..tostring(r)..","..tostring(g)..","..tostring(b).."> }")
    end
    local x1,x2,y1,y2,z1,z2 = table.unpack(self.param.viewport3d)
    local dist, N = pt3d.abs(M(x2-x1,y2-y1,z2-z1)), self.Normal
    pov_writeln("camera{ orthographic")
    pov_writeln("     location "..tostring(1000*dist).."*<"..tostring(-N.x)..","..tostring(N.y)..","..tostring(N.z)..">")
    local SkyY = self:ScreenY()
    local SkyX = self:ScreenX()
    local centerBox = (self.Xmax+self.Xmin)/2*SkyX + (self.Ymax+self.Ymin)/2*SkyY
    pov_writeln("     sky <"..tostring(self.cosPhi*self.cosTheta)..","..tostring(-self.cosPhi*self.sinTheta)..","..tostring(self.sinPhi)..">")
    pov_writeln("     up "..tostring(self.Ymax-self.Ymin).."*y")
    pov_writeln("     right ".. tostring(self.Xmax-self.Xmin).."*x")
    pov_writeln("     look_at <0,0,0>")
    pov_writeln("     translate <"..tostring(-centerBox.x)..","..tostring(centerBox.y)..","..tostring(centerBox.z)..">}")
    local light = pt3d.normalize( M(-self.cosPhi*self.cosTheta/2+self.sinTheta/3-N.x, -self.cosPhi*self.sinTheta/2-self.cosTheta/3+N.y, self.sinPhi/2+N.z))
    local str_light = "light_source { "..tostring(1000*dist).."*<"..tostring(light.x)..","..tostring(light.y)..","..tostring(light.z).."> color rgb<1,1,1>"
    if Pov_shadow == false then str_light =  str_light.." shadowless" end 
    pov_writeln( str_light.."}")
    Pov_export = Pov_declarations
end

function graph3d:Pov_strdot(A)
    return strdot(A)
end

------------------------------- implicit surf f(x,y,z)=0 ------------------------------

function graph3d:Pov_implicit( povfunc, luafunc, options)
-- luafunc : lua function of (x,y,z), to draw luafunc(x,y,z)=0
-- povfunc : povray version of the function (string)
-- options = {clipbox= default, clipplane, render=1, name=default, matrix=nil, color=White (rgb table), opacity=1, ambient=0.35, diffuse=0.8, phong=0.5, shadow=true, mytexture=nil,containedby = default win 3d }
    options = options or {}
    local render = options.render
    if render == nil then render = true end
    local name = options.name or "object"..get_num()
    local matrix = options.matrix
    local color = options.color or White
    local opacity = options.opacity or 1
    local ambient = options.ambient or 0.35
    local diffuse = options.diffuse or 0.8
    local phong = options.phong or 0.5
    local shadow = options.shadow
    if shadow == nil then shadow = true end
    if matrix == nil then matrix = self.matrix3d
    else
        matrix = composematrix3d(self.matrix3d,matrix)
    end
    local containedby = options.containedby
    if containedby == nil then 
        local x1,x2,y1,y2,z1,z2 = table.unpack(self.param.viewport3d)
        containedby = {M(x1,y1,z1), M(x2,y2,z2)}
    end
    
    local max_grad = function(f, box)
        local M1, M2 
        if type(box[2]) == "number" then 
            local C, r = table.unpack(box)
            M1, M2 = M(C.x-r,C.y-r,C.z-r), M(C.x+r,C.y+r,C.z+r)
        else M1, M2 = table.unpack(box)
        end
        local x1,x2,y1,y2,z1,z2 = M1.x, M2.x, M1.y, M2.y, M1.z, M2.z
        local nb = 20
        local pasx, pasy, pasz = (x2-x1)/(nb-1), (y2-y1)/(nb-1), (z2-z1)/(nb-1)
        local min, max = math.huge, -math.huge
        for x = x1, x2, pasx do
                for y = y1, y2, pasy do
                    for z = z1, z2, pasz do
                            local m = math.abs(f(x,y,z))
                            if max <m then max = m end
                            if min > m then min = m end
                    end
                end
        end
        return tostring((max+min)/3)
    end
    
    local M1, M2 = table.unpack( containedby )
    pov_writeln("\n#declare "..name.." =  isosurface{ ")
    pov_writeln("   function{"..povfunc.."}")
    if isPoint3d(M2) then 
        pov_writeln("    contained_by{ box{ ".. strdot2(M1).." "..strdot2(M2).."} }")
    else
         pov_writeln("    contained_by{ sphere{ ".. strdot2(M1)..", ".. tostring(M2).."} }")
    end
    pov_writeln("    open") 
    pov_writeln("    evaluate "..max_grad(luafunc,containedby)..", 1.5, 0.7") 
    write_matrix2(matrix)
    if options.clipbox ~= nil then  write_clip(options.clipbox, true) end
    if options.clipplane ~= nil then  write_clip(options.clipplane,false) end
    if render then
        write_rendering(name,color,opacity,ambient,diffuse,phong,shadow,options.mytexture)
    end
    pov_writeln("  }")
end


--------------- parametric surface ----------------------------

function graph3d:Pov_surface( xfunc, yfunc, zfunc, u1, u2, v1, v2, options)
-- xfunc = string representing x(u,v), yfunc = string representing y(u,v), zfunc = string representing z(u,v)
-- options = {clipbox= default, clipplane, render=1, name=default, matrix=nil, color=White (rgb table), opacity=1, ambient=0.35, diffuse=0.8, phong=0.5, shadow=true, mytexture=nil, max_grad=default, containedby=default win3d}
    options = options or {}
    local render = options.render
    if render == nil then render = true end
    local name = options.name or "object"..get_num()
    local matrix = options.matrix
    local color = options.color or White
    local opacity = options.opacity or 1
    local ambient = options.ambient or 0.35
    local diffuse = options.diffuse or 0.8
    local phong = options.phong or 0.5
    local shadow = options.shadow
    local max_grad = options.max_grad
    if shadow == nil then shadow = true end
    if matrix == nil then matrix = self.matrix3d
    else
        matrix = composematrix3d(self.matrix3d,matrix)
    end
    local containedby = options.containedby
    if containedby == nil then 
        local x1,x2,y1,y2,z1,z2 = table.unpack(self.param.viewport3d)
        containedby = {M(x1,y1,z1), M(x2,y2,z2)}
    end
    local M1, M2 = table.unpack( containedby )
    pov_writeln("\n#declare "..name.." = parametric{")
    pov_writeln("    function{"..xfunc.."},")
    pov_writeln("    function{"..yfunc.."},")
    pov_writeln("    function{"..zfunc.."}")
    pov_writeln("    <"..tostring(u1)..","..tostring(v1)..">, <"..tostring(u2)..','..tostring(v2)..">")
    if isPoint3d(M2) then 
        pov_writeln("    contained_by{ box{ ".. strdot2(M1).." ".. strdot2(M2).."} }")
    else
         pov_writeln("    contained_by{ sphere{ ".. strdot2(M1)..", ".. tostring(M2).."} }")
    end
    if max_grad ~= nil then
        pov_writeln("    max_gradient "..tostring(max_grad)) 
    end
    write_matrix2(matrix)
    if options.clipbox ~= nil then  write_clip(options.clipbox, true) end
    if options.clipplane ~= nil then  write_clip(options.clipplane,false) end
    if render then
        write_rendering(name,color,opacity,ambient,diffuse,phong,shadow,options.mytexture)
    end
    pov_writeln("  }")
end

------------------- plane ------------------------------------------
function graph3d:Pov_plane(P, options) -- plane as facet
-- P = {A, N} A = dot on plane, N = normal vector
-- options = -- Pov_facet options, plus scale=1
    if P == nil then return  end
    options = options or {}
    options.scale = options.scale or 1
    local matrix = options.matrix or ID3d
    matrix = composematrix3d(self.matrix3d,matrix)
    local oldmatrix = self.matrix3d
    self.matrix3d = ID3d
    local A, n = table.unpack(P)
    if not isID3d(matrix) then
        A = mtransform3d(A,matrix)
        n = mLtransform3d(n,matrix)
    end
    options.matrix = ID3d
    local face = self:Plane2facet({A,n},options.scale)
    if face ~= nil then 
        self:Pov_facet(face, options)
    end
    self.matrix3d = oldmatrix
end

function graph3d:Pov_plane2( P, options)
-- P = {A, N} A = dot on plane, N = normal vector
-- options = {clipbox= nil, clipplane=nil, render=1, name=default, matrix=nil, color=White (rgb table), opacity=1, ambient=0.35, diffuse=0.8, phong=0.5, shadow=true, mytexture=nil}
    options = options or {}
    local render = options.render
    if render == nil then render = true end
    local name = options.name or "object"..get_num()
    local matrix = options.matrix
    local color = options.color or White
    local opacity = options.opacity or 1
    local ambient = options.ambient or 0.35
    local diffuse = options.diffuse or 0.8
    local phong = options.phong or 0.5
    local shadow = options.shadow
    if shadow == nil then shadow = true end
    if matrix == nil then matrix = self.matrix3d
    else
        matrix = composematrix3d(self.matrix3d,matrix)
    end
    local x1,x2,y1,y2,z1,z2 = table.unpack(self.param.viewport3d)
    if options.clipbox == nil then 
        options.clipbox = {M(x1,y1,z1), M(x2,y2,z2)}
    else
        table.insert(options.clipbox,1,{M(x1,y1,z1), M(x2,y2,z2)})
    end
    local A, N = table.unpack( P )
    N = pt3d.normalize(N)
    local O = proj3d(Origin, P)
    local d = pt3d.abs(O)
    if pt3d.dot(O,N) < 0 then d = -d end -- distance à l'origine
    pov_writeln("\n#declare "..name.." = plane{")
    pov_writeln("    "..strdot(N)..", "..tostring(d))    
    write_matrix(matrix)
    if options.clipbox ~= nil then  write_clip(options.clipbox, true) end
    if options.clipplane ~= nil then  write_clip(options.clipplane, false) end
    if render then
        write_rendering(name,color,opacity,ambient,diffuse,phong,shadow,options.mytexture)
    end
    pov_writeln("  }")    
end


-------------------------- facets -----------------------------------

function graph3d:Pov_facet(F,options)
-- F: polyhedron or list of facets
-- options = {clipbox= nil, clipplane=nil, render=1, name=default, matrix=nil, color=White (rgb table), opacity=1, ambient=0.35, diffuse=0.8, phong=0.5, shadow=true, mytexture=nil, edge=false, edgecolor=default, edgewidth=default, hidden=default, hiddenstyle=default}
    options = options or {}
    if F == nil then return end
    if isPoint3d(F[1]) then F = {F} end
    local render = options.render
    if render == nil then render = true end
    local name = options.name or "object"..get_num()
    local matrix = options.matrix
    local color = options.color or White
    local opacity = options.opacity or 1
    local ambient = options.ambient or 0.35
    local diffuse = options.diffuse or 0.8
    local phong = options.phong or 0.5
    local shadow = options.shadow
    if shadow == nil then shadow = true end
    if matrix == nil then matrix = self.matrix3d
    else
        matrix = composematrix3d(self.matrix3d,matrix)
    end
    local edge = options.edge or false
    local edgecolor = options.edgecolor or Black
    local edgewidth = options.edgewidth or self.param.linewidth
    local hidden = options.hidden or Hiddenlines
    local hiddenstyle = options.hiddenstyle or Hiddenlinestyle
    local triangulate = function(F) --triangulate facets or a polyhedron, returns polyhedron
        if F == nil then return end
        local rep = {}
        if F.vertices ~= nil then --polyhedron
            rep.vertices = table.copy(F.vertices)
            rep.facets = {}
            for _,f in ipairs(F.facets) do
                local a, c, b = f[1], f[2]
                for k = 3, #f do
                    b = c; c = f[k]
                    table.insert(rep.facets,{a,b,c})
                end
            end
        else  -- facets
            for _,f in ipairs(F) do
                local a, c, b = f[1], f[2]
                for k = 3, #f do
                    b = c; c = f[k]
                    table.insert(rep,{a,b,c})
                end
            end
            rep = facet2poly(rep)
        end
        return rep
    end
    
    local F1 = triangulate(F)
    pov_writeln("\n#declare "..name.." = mesh2{")
    pov_writeln("         vertex_vectors{ "..tostring(#F1.vertices)..",")
    local sep = ""
    for _, S in ipairs(F1.vertices) do
        pov_writeln("          "..sep..strdot(S)); sep = ","
    end
    pov_writeln("                       }")
    pov_writeln("         face_indices{ "..tostring(#F1.facets)..",")
    sep = ""
    for _,f in ipairs(F1.facets) do
        pov_writeln("           "..sep.."<"..tostring(f[1]-1)..","..tostring(f[2]-1)..","..tostring(f[3]-1)..">"); sep = ","
    end
    pov_writeln("                       }")
    write_matrix(matrix)
    if options.clipbox ~= nil then  write_clip(options.clipbox, true) end
    if options.clipplane ~= nil then  write_clip(options.clipplane, false) end
    if render then
        write_rendering(name,color,opacity,ambient,diffuse,phong,shadow,options.mytexture)
    end
    pov_writeln("  }")
    if edge then
        local L = facetedges(F)
        whatis(F)
        self:Pov_polyline(L, {color=edgecolor, style=edgestyle, hidden=hidden, hiddenstyle=hiddenstyle, matrix=options.matrix, clipbox = options.clipbox, clipplane=options.clipplane, shadow=options.shadow})
    end
end


------------------------- geometric forms ----------------------------
function graph3d:Pov_box(A,B,options)
-- A = M(xmin,ymin,zmin), B = M(xmax,ymax,zmax)
-- options = {clipbox= nil, clipplane=nil, render=1, name=default, matrix=nil, color=White (rgb table), opacity=1, ambient=0.35, diffuse=0.8, phong=0.5, shadow=true, mytexture=nil}
    options = options or {}
    local render = options.render
    if render == nil then render = true end
    local name = options.name or "object"..get_num()
    local matrix = options.matrix
    local color = options.color or White
    local opacity = options.opacity or 1
    local ambient = options.ambient or 0.35
    local diffuse = options.diffuse or 0.8
    local phong = options.phong or 0.5
    local shadow = options.shadow
    if shadow == nil then shadow = true end
    if matrix == nil then matrix = self.matrix3d
    else
        matrix = composematrix3d(self.matrix3d,matrix)
    end
    pov_writeln("\n#declare "..name.." = box{")
    pov_writeln("    "..strdot(A).." "..strdot(B))
    write_matrix(matrix)
    if options.clipbox ~= nil then  write_clip(options.clipbox, true) end
    if options.clipplane ~= nil then  write_clip(options.clipplane, false) end
    if render then
        write_rendering(name,color,opacity,ambient,diffuse,phong,shadow,options.mytexture)
    end
    pov_writeln("  }")
end

function graph3d:Pov_torus( A,R,r,N,options)
-- A = center, R = big radius, r = small radius, N = normal vector
-- options = {clipbox= nil, clipplane=nil, render=1, name=default, matrix=nil, color=White (rgb table), opacity=1, ambient=0.35, diffuse=0.8, phong=0.5, shadow=true, mytexture=nil}
    options = options or {}
    local render = options.render
    if render == nil then render = true end
    local name = options.name or "object"..get_num()
    local matrix = options.matrix
    local color = options.color or White
    local opacity = options.opacity or 1
    local ambient = options.ambient or 0.35
    local diffuse = options.diffuse or 0.8
    local phong = options.phong or 0.5
    local shadow = options.shadow
    if shadow == nil then shadow = true end
    if matrix == nil then matrix = self.matrix3d
    else
        matrix = composematrix3d(self.matrix3d,matrix)
    end
    local V, mat = pt3d.normalize(N) -- mat = matrix to correctly orient the torus
    if math.abs(V.y) == 1 then 
        mat = {A,vecI,vecJ,vecK} 
    else
        mat = matrix3dof( function(X) return A+rotate3d(X, math.acos(V.y)*rad,{Origin, pt3d.prod(vecJ,V)}) end)
    end
    matrix = composematrix3d(matrix, mat)
    pov_writeln("\n#declare "..name.." = torus{")
    pov_writeln("    ".. tostring(R)..", "..tostring(r))
    write_matrix(matrix)
    if options.clipbox ~= nil then  write_clip(options.clipbox, true) end
    if options.clipplane ~= nil then  write_clip(options.clipplane, false) end
    if render then
        write_rendering(name,color,opacity,ambient,diffuse,phong,shadow,options.mytexture)
    end
    pov_writeln("  }")    
end

function graph3d:Pov_sphere(A,R,options)
-- A = center, R = big radius, r = small radius, N = normal vector
-- options = {clipbox= nil, clipplane=nil, render=1, name=default, matrix=nil, color=White (rgb table), opacity=1, ambient=0.35, diffuse=0.8, phong=0.5, shadow=true, mytexture=nil}
    options = options or {}
    local render = options.render
    if render == nil then render = true end
    local name = options.name or "object"..get_num()
    local matrix = options.matrix
    local color = options.color or White
    local opacity = options.opacity or 1
    local ambient = options.ambient or 0.35
    local diffuse = options.diffuse or 0.8
    local phong = options.phong or 0.5
    local shadow = options.shadow
    if shadow == nil then shadow = true end
    if matrix == nil then matrix = self.matrix3d
    else
        matrix = composematrix3d(self.matrix3d,matrix)
    end
    pov_writeln("\n#declare "..name.." = sphere{")
    pov_writeln("    "..strdot(A).." "..tostring(R))
    write_matrix(matrix)
    if options.clipbox ~= nil then  write_clip(options.clipbox, true) end
    if options.clipplane ~= nil then  write_clip(options.clipplane, false) end
    if render then
        write_rendering(name,color,opacity,ambient,diffuse,phong,shadow,options.mytexture)
    end
    pov_writeln("  }")
end


function graph3d:Pov_cylinder(A,R,B, options)
-- A = center, R = big radius, r = small radius
-- options = {clipbox= nil, clipplane=nil, render=1, name=default, matrix=nil, color=White (rgb table), opacity=1, ambient=0.35, diffuse=0.8, phong=0.5, shadow=true, mytexture=nil, hollow=false}
    options = options or {}
    local render = options.render
    if render == nil then render = true end
    local name = options.name or "object"..get_num()
    local matrix = options.matrix
    local color = options.color or White
    local opacity = options.opacity or 1
    local ambient = options.ambient or 0.35
    local diffuse = options.diffuse or 0.8
    local phong = options.phong or 0.5
    local hollow = options.hollow or false
    local shadow = options.shadow
    if shadow == nil then shadow = true end
    if matrix == nil then matrix = self.matrix3d
    else
        matrix = composematrix3d(self.matrix3d,matrix)
    end
    pov_writeln("\n#declare "..name.." = cylinder{")
    pov_writeln("    "..strdot(A).." "..strdot(B).." "..tostring(R))
    if hollow then pov_writeln("    open") end
    write_matrix(matrix)
    if options.clipbox ~= nil then  write_clip(options.clipbox, true) end
    if options.clipplane ~= nil then  write_clip(options.clipplane, false) end
    if render then
        write_rendering(name,color,opacity,ambient,diffuse,phong,shadow,options.mytexture)
    end
    pov_writeln("  }")
end

function graph3d:Pov_cone(A,R,B,r,options)
-- A = center, R = big radius, r = small radius
-- options = {clipbox= nil, clipplane=nil, render=1, name=default, matrix=nil, color=White (rgb table), opacity=1, ambient=0.35, diffuse=0.8, phong=0.5, shadow=true, mytexture=nil, hollow=false}
    if type(r) == "table" then options = r; r = 0 end
    options = options or {}
    local render = options.render
    if render == nil then render = true end
    local name = options.name or "object"..get_num()
    local matrix = options.matrix
    local color = options.color or White
    local opacity = options.opacity or 1
    local ambient = options.ambient or 0.35
    local diffuse = options.diffuse or 0.8
    local phong = options.phong or 0.5
    local hollow = options.hollow or false
    local shadow = options.shadow
    if shadow == nil then shadow = true end
    if matrix == nil then matrix = self.matrix3d
    else
        matrix = composematrix3d(self.matrix3d,matrix)
    end
    pov_writeln("\n#declare "..name.." = cone{")
    pov_writeln("    "..strdot(A).." "..tostring(R).." "..strdot(B).." "..tostring(r))
    if hollow then pov_writeln("    open") end
    write_matrix(matrix)
    if options.clipbox ~= nil then  write_clip(options.clipbox, true) end
    if options.clipplane ~= nil then  write_clip(options.clipplane, false) end
    if render then
        write_rendering(name,color,opacity,ambient,diffuse,phong,shadow,options.mytexture)
    end
    pov_writeln("  }")
end

function graph3d:Pov_circle(A,R,N,options)
-- A = center, R = radius, N = normal vector
-- options = {clipbox= nil, clipplane=nil, render=1, name=default, matrix=nil, color=White (rgb table), opacity=1, ambient=0.35, diffuse=0.8, phong=0.5, shadow=true, mytexture=nil, width=8}
    options = options or {}
    local width = options.width or self.param.linewidth
    options.color = options.color or Black
    local r = width*pt/10
    self:Pov_torus(A,R,r,N,options)
end


----------------------- polyline ---------------------------------------
function graph3d:Pov_polyline(L, options)
-- L list of 3D points or list of lists of 3D points
-- options = {clipbox= nil, clipplane=nil, render=1, name=default, matrix=nil, color=White (rgb table), opacity=1, ambient=0.35, diffuse=0.8, phong=0.5, shadow=true, mytexture=nil, width=8, close=false, arrows=0, hidden=default,hiddenstyle=defaut}
    if L == nil then return end
    options = options or {}
    local render = options.render
    if render == nil then render = true end
    local name = options.name or "object"..get_num()
    local matrix = options.matrix
    local color = options.color or Black
    local opacity = options.opacity or self.param.lineopacity
    local ambient = options.ambient or 0.35
    local diffuse = options.diffuse or 0.8
    local phong = options.phong or 0.5
    local hollow = options.hollow or false
    local shadow = options.shadow
    if shadow == nil then shadow = true end
    if matrix == nil then matrix = self.matrix3d
    else
        matrix = composematrix3d(self.matrix3d,matrix)
    end
    local width = options.width or self.param.linewidth
    width = width*pt/10
    local arrows = options.arrows or 0
    local arrowscale = options.arrowscale or 1
    local close = options.close or false
    local style = options.style or self.param.linestyle
    if style == "noline" then return end
    local hidden = options.hidden or Hiddenlines
    local hiddenstyle = options.hiddenstyle or Hiddenlinestyle

    if isPoint3d(L[1]) then L = {L} end
    if not isID3d(matrix) then L = mtransform3d(L,matrix) end
    local listarrows, listdots
    local arrows_radius = arrowscale*(width+0.075)
    
    local prepare_cp = function(cp)
        local aux = table.copy(cp)
        if close and (aux[1] ~= aux[#aux]) then table.insert(aux,aux[1]) end
        local A, B, h, d
        listarrows = {}
        local n = #aux
        if arrows >= 1 then
            A = aux[n-1]; B = table.remove(aux)
            d = pt3d.abs(B-A); h = arrows_radius*3*pt3d.normalize(A-B)--arrowscale*0.25*pt3d.normalize(A-B)
            if d >= arrowscale*0.25 then table.insert(aux,B+h) end
            table.insert(listarrows,{B+h,arrows_radius,B})
        end
        if arrows == 2 then
            A = table.remove(aux,1); B = aux[1]
            d = pt3d.abs(B-A); h = arrowscale*0.25*pt3d.normalize(B-A)
            if d >= arrowscale*0.25 then table.insert(aux,A+h) end
            table.insert(listarrows,{A+h,arrows_radius,A})
        end
        listdots = aux
    end
    
    if options.clippbox ~= nil then 
        local M1,M2 = table.unpack( options.clippbox )
        local poly = parallelep(M1, (M2.x-M1.x)*vecI,(M2.y-M1.y)*vecJ,(M2.z-M1.z)*vecK)
        L = clippolyline3d(L, poly, false, close)
    end
    if options.clipplane ~= nil then 
        L = cutpolyline3d(L,options.clipplane,close)
    end
    pov_writeln("\n#declare "..name.." =  union{")
    for _, cp in ipairs(L) do
        prepare_cp(cp)
        for _,arr in ipairs(listarrows) do
            pov_writeln("    cone{ "..strdot(arr[1]).." "..tostring(arr[2]))
            pov_writeln("           "..strdot(arr[3]).." 0 }")
        end
        local B, A = listdots[1]
        if style == "solid" then
            for k = 2, #listdots do
                A = B; B = listdots[k]
                if pt3d.abs(B-A) > 1e-10 then
                    pov_writeln("    cylinder{"..strdot(A)..","..strdot(B).." "..tostring(width).."}")
                else B = A
                end
            end
        elseif style == "dotted" then
            local d = 8*width
            for k = 2, #listdots do
                A = B; B = listdots[k]
                local norm = pt3d.abs(B-A)
                if pt3d.abs(B-A) > 1e-10 then
                    local u = d*(B-A)/norm
                    local N = math.floor(norm/d)+1
                    local dep = A
                    for p = 1, N do
                        pov_writeln("    sphere{"..strdot(dep).." "..tostring(width).."}")
                        dep = dep+u
                    end
                else B = A
                end
            end
        elseif style == "dashed" then
            local d = 8*width
            for k = 2, #listdots do
                A = B; B = listdots[k]
                local norm = pt3d.abs(B-A)
                if pt3d.abs(B-A) > 1e-10 then
                    local u = d*(B-A)/norm
                    local N = math.floor(pt3d.abs(B-A-4*u)/d/2)+1
                    local dep, fin = A, A+u/2
                    pov_writeln("    cylinder{"..strdot(dep)..","..strdot(fin).." "..tostring(width).."}")
                    dep = A+1.5*u; fin = dep+u
                    for p = 0, N do
                        pov_writeln("    cylinder{"..strdot(dep)..","..strdot(fin).." "..tostring(width).."}")
                        dep = dep+2*u; fin = fin+2*u
                    end
                    dep = B-u/2; fin = B
                    pov_writeln("    cylinder{"..strdot(dep)..","..strdot(fin).." "..tostring(width).."}")
                else B = A
                end
            end
        end
    end
    if render then
        write_rendering(name,color,opacity,ambient,diffuse,phong,shadow,options.mytexture)
    end
    pov_writeln("  }")
    if hidden then
        options.hidden = false; options.style = hiddenstyle
        self:Pov_polyline( shift3d(L,500*self.Normal), options)
    end
end

------------------- axes -----------------------------------------------
function graph3d:Pov_axes(O, options)
    local x0,y0,z0 = O.x, O.y, O.z
    local rep = {}
    local x1,x2,y1,y2,z1,z2 = table.unpack(self.param.viewport3d)
    self:Pov_polyline({{M(x1,y0,z0),M(x2,y0,z0)}, {M(x0,y1,z0),M(x0,y2,z0)},{M(x0,y0,z1),M(x0,y0,z2)}}, options)
end

------------------ dots ------------------------------------------------
function graph3d:Pov_dots(L, options)
-- L = list of 3d points
-- options = {clipbox= nil, clipplane=nil, render=1, name=default, matrix=nil, color=White (rgb table), opacity=1, ambient=0.35, diffuse=0.8, phong=0.5, shadow=true, mytexture=nil,style="ball" or "box", dotscale=1}
    if L == nil then return end
    if isPoint3d(L) then L = {L} end
    options = options or {}
    local render = options.render
    if render == nil then render = true end
    local name = options.name or "object"..get_num()
    local color = options.color or Black
    local style = options.style or "ball"
    local dotscale = options.dotscale or 1
    local opacity = options.opacity or self.param.lineopacity
    local ambient = options.ambient or 0.35
    local diffuse = options.diffuse or 0.8
    local phong = options.phong or 0.5
    local hollow = options.hollow or false
    local shadow = options.shadow
    if shadow == nil then shadow = true end
    local matrix = options.matrix    
    if matrix == nil then matrix = self.matrix3d
    else
        matrix = composematrix3d(self.matrix3d,matrix)
    end
    if not isID3d(matrix) then L = mtransform3d(L,matrix) end
    local deb, fin = "", ""
    if #L > 1 then deb = "union{"; fin = "}" end
    pov_writeln("\n#declare "..name.." =  "..deb)
    if style == "ball" then
        local r = dotscale*0.075
        for _, A in ipairs(L) do 
            pov_writeln("    sphere{"..strdot(A).." "..tostring(r)..fin)
        end
    elseif style == "box" then
        local d = dotscale*0.15/2
        for _, A in ipairs(L) do 
            pov_writeln("    box{"..strdot(M(A.x-d,A.y-d,A.z-d)).." "..strdot(M(A.x+d,A.y+d,A.z+d))..fin)
        end
    end
    if options.clipbox ~= nil then  write_clip(options.clipbox, true) end
    if options.clipplane ~= nil then  write_clip(options.clipplane, false) end    
    if render then
        write_rendering(name,color,opacity,ambient,diffuse,phong,shadow,options.mytexture)
    end
    pov_writeln("  }")
end

----------------- CSG : union, intersection, difference, merge ---------
function graph3d:Pov_union(list, options)
-- options = {clipbox= nil, clipplane=nil, render=1, name=default, matrix=nil, color=White (rgb table), opacity=1, ambient=0.35, diffuse=0.8, phong=0.5, shadow=true, mytexture=nil}
    if type(r) == "table" then options = r; r = 0 end
    options = options or {}
    local render = options.render
    if render == nil then render = true end
    local name = options.name or "object"..get_num()
    local matrix = options.matrix
    local color = options.color or White
    local opacity = options.opacity or 1
    local ambient = options.ambient or 0.35
    local diffuse = options.diffuse or 0.8
    local phong = options.phong or 0.5
    local hollow = options.hollow or false
    local shadow = options.shadow
    if shadow == nil then shadow = true end
    if matrix == nil then matrix = self.matrix3d
    else
        matrix = composematrix3d(self.matrix3d,matrix)
    end
    pov_writeln("\n#declare "..name.." = union{")
    for _,obj in ipairs(list) do
        pov_writeln("    object { "..obj.." }")
    end
    write_matrix(matrix)
    if options.clipbox ~= nil then  write_clip(options.clipbox, true) end
    if options.clipplane ~= nil then  write_clip(options.clipplane, false) end
    if render then
        write_rendering(name,color,opacity,ambient,diffuse,phong,shadow,options.mytexture)
    end
    pov_writeln("  }")
end

function graph3d:Pov_intersection(list, options)
-- options = {clipbox= nil, clipplane=nil, render=1, name=default, matrix=nil, color=White (rgb table), opacity=1, ambient=0.35, diffuse=0.8, phong=0.5, shadow=true, mytexture=nil, hollow=false}
    if type(r) == "table" then options = r; r = 0 end
    options = options or {}
    local render = options.render
    if render == nil then render = true end
    local name = options.name or "object"..get_num()
    local matrix = options.matrix
    local color = options.color or White
    local opacity = options.opacity or 1
    local ambient = options.ambient or 0.35
    local diffuse = options.diffuse or 0.8
    local phong = options.phong or 0.5
    local hollow = options.hollow or false
    local shadow = options.shadow
    if shadow == nil then shadow = true end
    if matrix == nil then matrix = self.matrix3d
    else
        matrix = composematrix3d(self.matrix3d,matrix)
    end
    pov_writeln("\n#declare "..name.." = intersection{")
    for _,obj in ipairs(list) do
        pov_writeln("    object { "..obj.." }")
    end
    write_matrix(matrix)
    if options.clipbox ~= nil then  write_clip(options.clipbox, true) end
    if options.clipplane ~= nil then  write_clip(options.clipplane, false) end
    if render then
        write_rendering(name,color,opacity,ambient,diffuse,phong,shadow,options.mytexture)
    end
    pov_writeln("  }")
end

function graph3d:Pov_merge(list, options)
-- options = {clipbox= nil, clipplane=nil, render=1, name=default, matrix=nil, color=White (rgb table), opacity=1, ambient=0.35, diffuse=0.8, phong=0.5, shadow=true, mytexture=nil, hollow=false}
    if type(r) == "table" then options = r; r = 0 end
    options = options or {}
    local render = options.render
    if render == nil then render = true end
    local name = options.name or "object"..get_num()
    local matrix = options.matrix
    local color = options.color or White
    local opacity = options.opacity or 1
    local ambient = options.ambient or 0.35
    local diffuse = options.diffuse or 0.8
    local phong = options.phong or 0.5
    local hollow = options.hollow or false
    local shadow = options.shadow
    if shadow == nil then shadow = true end
    if matrix == nil then matrix = self.matrix3d
    else
        matrix = composematrix3d(self.matrix3d,matrix)
    end
    pov_writeln("\n#declare "..name.." = merge{")
    for _,obj in ipairs(list) do
        pov_writeln("    object { "..obj.." }")
    end
    write_matrix(matrix)
    if options.clipbox ~= nil then  write_clip(options.clipbox, true) end
    if options.clipplane ~= nil then  write_clip(options.clipplane, false) end
    if render then
        write_rendering(name,color,opacity,ambient,diffuse,phong,shadow,options.mytexture)
    end
    pov_writeln("  }")
end

function graph3d:Pov_difference(list, options)
-- options = {clipbox= nil, clipplane=nil, render=1, name=default, matrix=nil, color=White (rgb table), opacity=1, ambient=0.35, diffuse=0.8, phong=0.5, shadow=true, mytexture=nil, hollow=false}
    if type(r) == "table" then options = r; r = 0 end
    options = options or {}
    local render = options.render
    if render == nil then render = true end
    local name = options.name or "object"..get_num()
    local matrix = options.matrix
    local color = options.color or White
    local opacity = options.opacity or 1
    local ambient = options.ambient or 0.35
    local diffuse = options.diffuse or 0.8
    local phong = options.phong or 0.5
    local hollow = options.hollow or false
    local shadow = options.shadow
    if shadow == nil then shadow = true end
    if matrix == nil then matrix = self.matrix3d
    else
        matrix = composematrix3d(self.matrix3d,matrix)
    end
    pov_writeln("\n#declare "..name.." = difference{")
    pov_writeln("    object { "..list[1].." }")
    pov_writeln("    object { "..list[2].." }")
    write_matrix(matrix)
    if options.clipbox ~= nil then  write_clip(options.clipbox, true) end
    if options.clipplane ~= nil then  write_clip(options.clipplane, false) end
    if render then
        write_rendering(name,color,opacity,ambient,diffuse,phong,shadow,options.mytexture)
    end
    pov_writeln("  }")
end

------------------- user ------------------------------------------
function graph3d:Pov_comment(comment) -- writes comment in pov source
    pov_writeln("//"..comment)
end    

function graph3d:Pov_special(user_content) -- written directly into the source code.
    pov_writeln(user_content)
end
