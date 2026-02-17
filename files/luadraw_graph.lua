-- luadraw_graph.lua (chargé par luadraw_graph2d.lua)
-- date 2026/02/17
-- version 2.6
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.

-- Tout ce qui touche au dessin de base, sans les axes
local luadraw_calc = require 'luadraw_calc'

local luadraw_graph = {}
setmetatable(luadraw_graph, {__index = luadraw_calc}) -- obligatoire pour l'héritage

--- Constructeur
function luadraw_graph:new(args) -- argument de la forme :
-- {window={x1,x2,y1,y2,xscale,yscale}, margin={left, right, top, bottom}, size={large, haut, ratio}, bg="color", border = true/false, bbox=true/false ,pictureoptions=""}
    local graph = luadraw_calc:new(args) -- obligatoire, on utilise le constructeur de luadraw_calc
    setmetatable(graph, {__index = luadraw_graph})  -- obligatoire, permet d'utiliser self
    
    graph.LF = "\\par" --line feed pour TeX
    graph.advancedexport = {""} -- export en début de graphique
    graph.advanced = false
    graph.currentexport = {""} -- export courant
    graph.deferredexport = {""} -- export en fin de graphique
    graph.deferred = false
    graph.export = graph.currentexport
    graph.pictureoptions = args.pictureoptions or ""
    
    -- styles par défaut
    graph.currentparam = {
            ["viewport"] = {graph.Xmin, graph.Xmax, graph.Ymin, graph.Ymax},  -- définit la vue courante
            ["coordsystem"] = {graph.Xmin, graph.Xmax, graph.Ymin, graph.Ymax},  -- coordonnées de la vue courante
            ["linestyle"] = "solid", -- "noline", ou "solid" ou "dashed" ou "dotted" ou  dash( motif )
            ["linecap"] = "butt", --"butt" ou "round" ou "square"
            ["linejoin"] = "miter", -- "miter" ou "round" ou "bevel"
            ["linewidth"]  = 4, -- en dixième de points
            ["linecolor"] = "black", -- couleur de tracé en svgnames ou tout autre nomenclature comprise par tikz
            ["lineopacity"] = 1,  -- opacité du trait
            ["arrows"] = "-", -- fleche, soit "-", soit "->" soit "<-" soit "<->"
            ["fillstyle"] = "none", --ou "full", "bdiag", "fdiag", "hvcross", "diagcross", "horizontal", "vertical", "gradient"
            ["fillcolor"] = "black",
            ["fillopacity"] = 1,
            ["filleo"] = false, -- even odd mode
            ["gradstyle"] = "left color = white, right color = red", -- gradient linéaire de gauche à droite
            ["dotstyle"] = "*", -- style par défaut (point rond)
            ["dotscale"] = 1, -- scale
            ["dotsize"] = Z(2,2), -- le diamètre du point est re + im*linewidth
            ["labelstyle"] = "center", -- position du label "center" ou "N" ou "S", ou "E", etc...
            ["labelcolor"] = "", -- couleur du  document  par défaut
            ["labelsize"] = "", --taille normale par défaut, ou "tiny", ou "small" ou ...
            ["labeldir"] = {}, -- direction de l'écriture (la table vide correspond au sens usuel)
            ["labelangle"] = 0 -- orientation du label
            }
    graph.advancedparam = table.copy(graph.currentparam)
    graph.deferredparam = table.copy(graph.currentparam)
    graph.param = graph.currentparam
    graph.tikzpictureoptions = "line join=round"
    if graph.border or (graph.bg ~= "") then
        graph.tikzpictureoptions = graph.tikzpictureoptions..",background rectangle/.style={"
        if graph.border then 
            graph.tikzpictureoptions = graph.tikzpictureoptions..",draw=black"
        end
        if graph.bg ~= "" then
            graph.tikzpictureoptions = graph.tikzpictureoptions..",fill="..graph.bg
        end
        graph.tikzpictureoptions = graph.tikzpictureoptions.."},show background rectangle"
    end
    if graph.pictureoptions ~= "" then
        graph.tikzpictureoptions = graph.tikzpictureoptions..","..graph.pictureoptions
    end
    graph.matrix = ID -- matrice de transformation [f(0), Lf(1), Lf(i) ], identité par défaut
    
    graph.pile = {} -- pour sauvegarder la fenêtre, les styles et la matrice
    graph.pilematrix = {} -- sauvegarde de la matrice uniquement
    return graph
end

function luadraw_graph:Coord(z)
--  z est un complexe et la fonction renvoie la chaîne "(x,y)" où x et y sont les coordonnées de z pour tikz
    if type(z) == "number" then return self:strCoord(z,0)
    else return self:strCoord(z.re,z.im)
    end
end

function luadraw_graph:Box2d()
-- renvoie la fenêtre 2d courante sous forme d'une ligne polygonale
    local x1,x2,y1,y2 = table.unpack(self.param.coordsystem)
    return {Z(x1,y1),Z(x2,y1),Z(x2,y2),Z(x1,y2)}
end

-- sauvegarde et restauration des paramètres graphiques (fenêtre, styles, matrice)
function luadraw_graph:Saveattr()
    table.insert(self.pile, table.copy(self.matrix))
    table.insert(self.pile, table.copy(self.param))
    self:Writeln("\\begin{scope}")
end

function luadraw_graph:Restoreattr()
    self.param = table.remove(self.pile)
    self.matrix = table.remove(self.pile)
    self:Writeln("\\end{scope}")
end

function luadraw_graph:Defaultattr()
    self:Lineoptions("solid","black",4,"-") -- "noline" ou "solid" ou "dashed" ou "dotted" ou  dash( motif )
    self:Linecap("butt") --"butt" ou "round" ou "square"
    self:Linejoin("miter") -- "miter" ou "round" ou "bevel"
    self:Lineopacity(1)  -- opacité du trait
    self:Filloptions("none","black",1,false) --ou "full" "bdiag" "fdiag" "hvcross" "diagcross" "horizontal" "vertical" "gradient"
    self:Gradstyle("left color=white, right color=red") -- gradient linéaire de gauche à droite
    self:Dotstyle("*") -- style par défaut (point rond)
    self:Dotscale(1) -- scale
    self:Labelstyle("center") -- position du label "center" ou "N" ou "S" ou "E" etc...
    self:Labelcolor("") -- couleur du  document  par défaut
    self:Labelsize("") --taille normale par défaut, ou "tiny" ou "small" ou ...
    self:Labelangle(0) -- orientation du label
end

function luadraw_graph:Cleargraph()
    self.advancedexport = {""} -- export en début de graphique
    self.advanced = false
    self.currentexport = {""} -- export courant
    self.deferredexport = {""} -- export en fin de graphique
    self.deferred = false
    self.export = self.currentexport
end

function luadraw_graph:Savematrix()
    table.insert(self.pilematrix, table.copy(self.matrix))
end

function luadraw_graph:Restorematrix()
    self.matrix = table.remove(self.pilematrix)
end

function luadraw_graph:Viewport(x1,x2,y1,y2) -- sélectionne une zone de la fenêtre initiale, il faut faire un g:SaveAttr() avant !
    if (x1 == nil) or (x2 == nil) or (y1 == nil) or (y2 == nil) then return end
    if x1 > x2 then x1, x2 = x2, x1 end
    if y1 > y2 then y1, y2 = y2, y1 end
    self.matrix = ID -- ancienne matrice perdue donc il faut sauver avant
    self.param.viewport = {x1,x2,y1,y2}
    self.param.coordsystem = {x1,x2,y1,y2}
    --self:Writeln("\\clip "..self:strCoord(x1,y1).." -- "..self:strCoord(x2,y1).." -- "..self:strCoord(x2,y2).." -- "..self:strCoord(x1,y2).."-- cycle;") --> clipping donc il faut sauver avant
end

function luadraw_graph:Coordsystem(x1,x2,y1,y2,ortho) -- nouveau système de coordonnées pour la vue courante
    if (x1 == nil) or (x2 == nil) or (y1 == nil) or (y2 == nil) then return end
    if x1 > x2 then x1, x2 = x2, x1 end
    if y1 > y2 then y1, y2 = y2, y1 end    
    local ort = (ortho or false)
    local X1,X2,Y1,Y2 = table.unpack(self.param.viewport) -- vue actuelle
    local G1 = Z((x1+x2)/2, (y1+y2)/2) -- centre du nouveau repérage
    local G2 = Z((X1+X2)/2, (Y1+Y2)/2) -- centre actuel
    local Dx = (X2-X1)/(x2-x1) -- réductions axes
    local Dy = (Y2-Y1)/(y2-y1)
    local D = nil
    if ort then
        if Dx > Dy then D = Dy else D = Dx end
        self:Setmatrix( {G2-G1*D, Z(D,0), Z(0,D) }) -- ancienne matrice perdue donc il faut sauver avant
        local A, B = (Z(X1,Y1)-G2)/D+G1, (Z(X2,Y2)-G2)/D+G1
        self.param.coordsystem = {A.re,B.re,A.im,B.im}
     else
        local M = { G2- Z(G1.re*Dx, G1.im*Dy), Z(Dx,0), Z(0,Dy) }
        self.param.coordsystem = {x1,x2,y1,y2}
        self:Setmatrix(M)
    end
end

function luadraw_graph:Getview()
    return self.param.viewport
end
function luadraw_graph:Xinf()
    return self.param.coordsystem[1]
end

function luadraw_graph:Xsup()
    return self.param.coordsystem[2]
end

function luadraw_graph:Yinf()
    return self.param.coordsystem[3]
end

function luadraw_graph:Ysup()
    return self.param.coordsystem[4]
end

-- écriture dans le document TeX ou dans un fichier pour TeX
function luadraw_graph:Write(str)
-- écrit la chaîne str sans retour à la ligne
    if str ~= "" then
        local n = #self.export
        self.export[n] = self.export[n]..str
    end
end

function luadraw_graph:Writeln(str)
--  écrit la chaîne str avec retour à la ligne
    self:Write(str)
    table.insert(self.export,"") -- on démarre une nouvelle ligne   
end

function luadraw_graph:Begindeferred()
-- pour repousser les instructions en fin de graphique
    self.deferred = true
    self.export = self.deferredexport
    self.param = self.deferredparam
end

function luadraw_graph:Enddeferred()
-- pour arrêter de repousser les instructions en fin de graphique
    self.export = self.currentexport
    self.param = self.currentparam
end

function luadraw_graph:Beginadvanced()
-- pour avancer les instructions en début de graphique
    self.advanced = true
    self.export = self.advancedexport
    self.param = self.advancedparam
end

function luadraw_graph:Endadvanced()
-- pour arrêter d'avancer les instructions en début de graphique
    self.export = self.currentexport
    self.param = self.currentparam
end


--afficher le graphique dans le document TeX  (utilisée par le fichier luadraw.sty pour la méthode Show())
function luadraw_graph:Sendtotex()
    --if #self.deferredexport ~= 0 then self:Defaultattr() end -- pour que les instructions décalées démarrent avec les paramètres par défaut
    local before, after = {""}, {""}
    if self.advanced then before = {"\\end{scope}%","\\begin{scope}%"} end
    if self.deferred then after = {"\\end{scope}%","\\begin{scope}%"} end
    local str = concat(self:beginDraw(),self.advancedexport,before,self.currentexport,after,self.deferredexport,self:endDraw())
    tex.sprint(table.unpack(str))
end

-- sauvegarde dans un fichier texte
function luadraw_graph:Savetofile(nom) 
  -- Ouverture du fichier en écriture seule
  -- Attention, si un fichier existe déjà avec ce nom, il sera écrasé !
    local file = io.open(nom, "w")
    if not file then
        print("I can't open the file "..nom)
        return
    end
    if #self.deferredexport ~= 0 then self:Defaultattr() end
  -- On écrit dans le fichier    
    for _, lg in ipairs(self:beginDraw()) do
        file:write(lg.."%\n")
    end
    for _, lg in ipairs(self.advancedexport) do
        if lg ~= "" then file:write(lg.."%\n") end
    end
    if self.advanced then 
        file:write("\\end{scope}%\n\\begin{scope}%\n") 
    end
    for _, lg in ipairs(self.currentexport) do
        if lg ~= "" then file:write(lg.."%\n") end
    end
    if self.deferred then 
        file:write("\\end{scope}%\n\\begin{scope}%\n")
    end    
    for _, lg in ipairs(self.deferredexport) do
        if lg ~= "" then file:write(lg.."%\n") end
    end
    for _, lg in ipairs(self:endDraw()) do
        file:write(lg.."%\n")
    end
    file:close()
end

function luadraw_graph:beginDraw() -- début du dessin
    local str = {"\\begin{tikzpicture}["..self.tikzpictureoptions.."]"}
     table.insert(str,"\\begin{scope}")
    if self.bbox then
        table.insert(str,"\\clip ("..tostring(-self.Lmargin)..",".. tostring(-self.Bmargin)..") rectangle ("..tostring(self:Graphwidth()+self.Rmargin)..","..tostring(self:Graphheight()+self.Tmargin)..");")
    end
    return str
end

function luadraw_graph:endDraw()  -- fin du dessin
    return {"\\end{scope}","\\end{tikzpicture}"}
end

------------- gestion des attributs de dessin --------------------------

local color2str = function(coul)
-- une couleur peut être soit une chaîne de caractères (un nom de couleur ou "{rgb,1:red,r;green,g;blue,b}" )
-- soit une table {r,g,b}
-- la  fonction renvoie une chaîne
    if coul == nil then return end
    local strCoul
    if type(coul) == "table" then -- c'est une table {r,g,b}
        strCoul = rgb(coul)
    else
        strCoul = coul
        if string.sub(strCoul,1,1) == "{" then -- on retire les accolades
            strCoul = string.sub(strCoul,2,#strCoul-1) 
        end 
    end
    return strCoul
end

function luadraw_graph:Linewidth(ep)
-- ep est une épaisseur en dixième de point ex 4 pour 0.4pt
    if self.param.linewidth ~= ep then
        self:Writeln("\\pgfsetlinewidth{"..tostring(0.1*ep).."pt}")
        self.param.linewidth = ep
    end
end

function luadraw_graph:Linecolor(strCoul) 
-- pour modifier la couleur du tracé (strCoul doit être une chaîne)
    strCoul = color2str(strCoul)
    if self.param.linecolor ~= strCoul then
        self:Writeln("\\pgfsetstrokecolor{"..strCoul.."}")
        self.param.linecolor = strCoul
    end
end

function luadraw_graph:Linecap(style) -- "butt" ou "round" ou "square"
    if self.param.linecap ~= style then
        if style == "butt" then
            self:Writeln("\\pgfsetbuttcap")
            self.param.linecap = style
        else if style == "round" then
                self:Writeln("\\pgfsetroundcap")
                self.param.linecap = style
            else
                if style == "square" then
                    self:Writeln("\\pgfsetrectcap")
                    self.param.linecap = style
                end
            end
        end
    end
end

function luadraw_graph:Linejoin(style) -- "miter" ou "round" ou "bevel"
    if self.param.linejoin ~= style then
        if style == "miter" then
            self:Writeln("\\pgfsetmiterjoin")
            self.param.linejoin = style
        else if style == "round" then
                self:Writeln("\\pgfsetroundjoin")
                self.param.linejoin = style
            else
                if style == "bevel" then
                    self:Writeln("\\pgfsetbeveljoin")
                    self.param.linejoin = style
                end
            end
        end
    end
end

function luadraw_graph:Dash(style) -- transforme par ex {2.5,2} en {2.5pt}{2pt}
    local str = ""
    for _,c in ipairs(style) do
        str = str.."{"..tostring(c).."pt}"
    end
    return str
end    

function luadraw_graph:Linestyle(style)
    if style == self.param.linestyle then return end
    if style == "noline" then
        self.param.linestyle = "noline"
    else
        if style == "solid" then
           self.param.linestyle = "solid" 
           self:Linecap("butt")
           self:Writeln("\\pgfsetdash{}{0pt}")
        else
            if style == "dashed" then
                self.param.linestyle = "dashed" 
                self:Linecap("butt")
                self:Writeln("\\pgfsetdash{{2.5pt}{2pt}}{0pt}")
            else
                if style == "dotted" then
                self.param.linestyle = "dotted"
                self:Linecap("round")
                self:Writeln("\\pgfsetdash{{0pt}{3pt}}{0pt}")
                else -- on attend un motif ex: "{2.5pt}{2pt}"
                    self.param.linestyle = style
                    self:Linecap("butt")
                    self:Writeln("\\pgfsetdash{"..style.."}{0pt}")
                end
            end
        end
    end
end

function luadraw_graph:Lineopacity(x) 
-- opacité du tracé, x est entre 0 et 1
    if x == self.param.lineopacity then return end
    x = (x or 1)
    if (0 <= x) and (x <= 1) then
        self.param.lineopacity = x
        self:Writeln("\\pgfsetstrokeopacity{"..x.."}")
    end
end
    
function luadraw_graph:Arrows(style) -- style = "->", "<-", "<->", nil
    if style == self.param.arrows then return end
    if style == "->" then self:Writeln("\\pgfsetarrows{-to}"); self.param.arrows = style
    else if style == "<-" then self:Writeln("\\pgfsetarrows{to-}"); self.param.arrows = style
        else if style == "<->" then self:Writeln("\\pgfsetarrows{to-to}"); self.param.arrows = style
            else 
                if style == "-" then
                    self:Writeln("\\pgfsetarrows{-}"); self.param.arrows = style
                else self:Writeln("\\pgfsetarrows{"..style.."}"); self.param.arrows = style
                end
            end
        end
    end
end

function luadraw_graph:Lineoptions(style,strCoul,width,arrows)
    if strCoul ~= nil then self:Linecolor(strCoul) end
    if style ~= nil then self:Linestyle(style) end
    if width ~= nil then self:Linewidth(width) end
    if arrows ~= nil then self:Arrows(arrows) end
end

function luadraw_graph:Filloptions(style,strCoul,opacity,evenOdd)
    if opacity ~= nil then self:Fillopacity(opacity) end
    if evenOdd ~= nil then self:Filleo(evenOdd) end
    if style == "gradient" then -- dans ce cas l'argument strCoul contient le style du gradient
        self.param.fillstyle = style
        if strCoul ~= nil then self:Gradstyle(strCoul) end
        return 
    end
    strCoul = color2str(strCoul)
    if strCoul == nil then strCoul = self.param.fillcolor end -- on ne change pas la couleur
    if style == nil then style = self.param.fillstyle end -- on ne change pas le style
    local ok = (self.param.fillcolor == strCoul)
    if not ok then
        self.param.fillcolor = strCoul
    end
    if (style == self.param.fillstyle) and ok then return end
    --if (style == "none") then self.param.fillstyle = style; return end    
    if (style == "none") or (style == "full") then 
        self.param.fillstyle = style 
        if (not ok) or (style == "full") then self:Writeln("\\pgfsetfillcolor{"..self.param.fillcolor.."}") end
        return 
    end
    if style == "bdiag" then self.param.fillstyle = style; self:Writeln("\\pgfsetfillpattern{north east lines}{"..self.param.fillcolor.."}"); return end
    if style == "hvcross" then self.param.fillstyle = style; self:Writeln("\\pgfsetfillpattern{grid}{"..self.param.fillcolor.."}"); return end
    if style == "diagcross" then self.param.fillstyle = style; self:Writeln("\\pgfsetfillpattern{crosshatch}{"..self.param.fillcolor.."}"); return end
    if style == "fdiag" then self.param.fillstyle = style; self:Writeln("\\pgfsetfillpattern{north west lines}{"..self.param.fillcolor.."}"); return end
    if style == "horizontal" then self.param.fillstyle = style; self:Writeln("\\pgfsetfillpattern{horizontal lines}{"..self.param.fillcolor.."}"); return end
    if style == "vertical" then self.param.fillstyle = style; self:Writeln("\\pgfsetfillpattern{vertical lines}{"..self.param.fillcolor.."}"); return end    
    self.param.fillstyle = style  -- en espérant que ce style existe !
    self:Writeln("\\pgfsetfillpattern{"..style.."}{"..self.param.fillcolor.."}")
end

function luadraw_graph:Fillopacity(x) 
-- opacité du tracé, x est entre 0 et 1
    if x == self.param.fillopacity then return end
    x = (x or 1)
    if (0 <= x) and (x <= 1) then
        self.param.fillopacity = x
        self:Writeln("\\pgfsetfillopacity{"..x.."}")
    end
end

function luadraw_graph:Filleo(evenOdd) 
-- opacité du tracé, x est entre 0 et 1
    evenOdd = (evenOdd or false)
    if (self.param.filleo ~= evenOdd) then
        self.param.filleo = evenOdd
        if evenOdd then self:Writeln("\\pgfseteorule") 
        else self:Writeln( "\\pgfsetnonzerorule");
        end
    end
end

function luadraw_graph:Labelstyle(position)
-- position du label par rapport au point d'ancrage
    position = position or ""
    self.param.labelstyle = position
end

function luadraw_graph:Labelcolor(color)
-- couleur du label, color est une chaîne représentant une couleur pour tikz
    color = color or "" -- la chaîne vide représente la couleur courante du document
    --color = color2str(color)
    self.param.labelcolor = color -- utilisée en local seulement
end

function luadraw_graph:Labelsize(size)
-- size est une chaîne : "normalsize" ou "small" ou "tiny" ou "huge" ... etc, la chaîne vide représente normalsize
    size = size or ""
    self.param.labelsize = size
end

function luadraw_graph:Labelangle(angle)
-- angle est en degré, rotation du label autour du point d'ancrage
    angle = angle or 0
    self.param.labelangle = angle
end

function luadraw_graph:Labeldir(dir)
-- dir est une table {dirX, dirY, dep} iniquant e sens de l'écriture, table vide par défaut
    dir = dir or {}
    self.param.labeldir = dir
end

function luadraw_graph:Gradstyle(chaine)
    -- définit le style de gradient, c'est une chaîne transmise telle quelle à \draw
    -- par défaut c'est "left color = white, right color = red"
    if type(chaine) == "string" then
        self.param.gradstyle = chaine
    end
end

function luadraw_graph:Dotstyle(style)
    style = (style or "*")
    if style == nil then return end
    if style ~= self.param.dotstyle then
        self.param.dotstyle = style
    end
end

function luadraw_graph:Dotscale(scale)
    scale = (scale or 1)
    if (scale == nil) or (type(scale) ~= "number") then return end
    if (scale > 0) and (scale ~= self.param.dotscale) then
        self.param.dotscale = scale
    end
end

-- dessins 

------------- ligne polygonale -----------------------------------------

function luadraw_graph:drawcmd(draw_options)
    local commande = ""
    local sep = ""
    draw_options = draw_options or ""
    if ((self.param.linestyle == "noline") and (self.param.fillstyle == "none")) then return end
    if (self.param.linestyle == "noline") then commande = "\\fill["  else commande = "\\draw[" end
    if (self.param.fillstyle ~= "none") and (self.param.linestyle ~= "noline") then commande = commande.."fill"; sep = "," end
    if self.param.fillstyle == "gradient" then commande = commande..sep..self.param.gradstyle; sep = "," end
    if draw_options ~= "" then commande = commande..sep..draw_options.."] "
    else commande = commande.."] "
    end
    return commande
end

function luadraw_graph:Dpolyline(L,close,draw_options,clip) -- close vaut true ou false
-- dessine une ligne polygonale avec les attributs courants
-- L est une liste de complexes ou une liste de listes de complexes
-- clip = {x1,x2,y1,y2} rectangle de clipping, ou nil  pour la fenêtre par défaut
    local clippee
    if (L == nil) or (type(L) ~= "table") or (#L == 0) then return end
    if type(close) == "string" then clip = draw_options; draw_options = close; close = false end
    close = close or false
    draw_options = draw_options or ""
    local commande = self:drawcmd(draw_options)
    if commande == nil then return end
    if (type(L[1]) == "number") or isComplex(L[1]) then L = {L} end
    local X1,X2,Y1,Y2
    if clip == nil then X1,X2,Y1,Y2 = table.unpack(self.param.viewport)
    else X1,X2,Y1,Y2 = table.unpack(clip)
    end
    for _, cp in ipairs(L) do
        clippe = false
        local len = #cp
        if len > 1 then
            if clip ~= nil then
                cp, clippee = clippolyline(cp,X1,X2,Y1,Y2,close)
            end
            if not isID(self.matrix) then
                cp = self:Mtransform(cp)
            end
            if clip == nil then cp, clippee = clippolyline(cp,X1,X2,Y1,Y2,close) end
            if (cp[1] ~= nil) and (not clippee) and close then table.remove(cp[1]) end
            for _, cpp in ipairs(cp) do
                len = #cpp
                if len > 1 then
                    local z = cpp[1]
                    self:Write(commande..self:Coord(z))
                    for k = 2, len do
                        z = cpp[k]
                        self:Write(" -- "..self:Coord(z))
                    end
                    if close then self:Writeln("--cycle;") 
                    else self:Writeln(";")
                    end
                end
            end
        end
    end
end

-- courbe cartésienne
function luadraw_graph:Dcartesian(f,args)
-- dessin d'une courbe cartésienne d'équation y=f(x) dans l'intervalle [x1;x2]
-- args est une table à 5 entrées args = { x = {x1,x2}, nbdots = 50, discont =false, nbdiv = 5, draw_options = "",clip={x1,x2,y1,y2} }
    args = args or {}
    local x = args.x or {self:Xinf(), self:Xsup()}
    local nbdots = args.nbdots or 40
    local discont = args.discont or false
    local nbdiv = args.nbdiv or 4
    local draw_options = args.draw_options or ""
    local x1, x2 = table.unpack(x)
    if x1 > x2 then x1, x2 = x2, x1 end
    local C = cartesian(f,x1,x2,nbdots,discont,nbdiv)
    self:Dpolyline(C,false,draw_options,args.clip)
end

-- courbe paramétrée
function luadraw_graph:Dparametric(p,args)
-- dessin d'une courbe paramétrée par la fonction p:t -> p(t) sur l'intervalle [t1;t2] (à valeurs complexes)
-- args est une table à 5 entrées args = { t = {t1,t2}, nbdots = 40, discont =false, nbdiv = 4, draw_options = "", clip={x1,x2,y1,y2} }
    args = args or {}
    local t = args.t or {self:Xinf(), self:Xsup()}
    local nbdots = args.nbdots or 40
    local discont = args.discont or false
    local nbdiv = args.nbdiv or 4
    local draw_options = args.draw_options or ""
    if draw_options == "" then draw_options = "line join=round"
    else draw_options = "line join=round,"..draw_options end -- jointure arrondie
    local t1, t2 = table.unpack(t)
    if t1 > t2 then t1, t2 = t2, t1 end
    local C = parametric(p,t1,t2,nbdots,discont,nbdiv)
    self:Dpolyline(C,false,draw_options,args.clip)
end

-- courbe polaire
function luadraw_graph:Dpolar(rho,args)
-- dessin d'une courbe polaire parmétrée par la fonction rho:t -> rho(t) sur l'intervalle [t1;t2] (à valeurs réelles)
-- args est une table à 6 entrées args = { t = {t1,t2}, nbdots = 40, discont =false, nbdiv = 4, draw_options = "", clip={x1,x2,y1,y2} } }
    args = args or {}
    local t = args.t or {-math.pi, math.pi}
    local nbdots = args.nbdots or 40
    local discont = args.discont or false
    local nbdiv = args.nbdiv or 4
    local draw_options = args.draw_options or ""
    local t1, t2 = table.unpack(t)
    if t1 > t2 then t1, t2 = t2, t1 end
    local C = polar(rho,t1,t2,nbdots,discont,nbdiv)
    self:Dpolyline(C,false,draw_options,args.clip)
end

-- courbe cartésienne périodique
function luadraw_graph:Dperiodic(f,period,args)
-- f est une fonction x -> f(x) réelle
-- period est une liste {a,b} avec a < b représentant une période
-- args est une table à 5 entrées args = { x = {x1,x2}, nbdots = 40, discont =false, nbdiv = 4, draw_options = "",clip={x1,x2,y1,y2} }
    if (period == nil) or (type(period) ~= "table") or (#period ~= 2) then return end
    args = args or {}
    local x = args.x or {self:Xinf(), self:Xsup()}
    local nbdots = args.nbdots or 40
    local discont = args.discont
    local nbdiv = args.nbdiv or 4
    local draw_options = args.draw_options or ""
    local x1, x2 = table.unpack(x)
    if x1 > x2 then x1, x2 = x2, x1 end
    local C = periodic(f,period,x1,x2,nbdots,discont,nbdiv)
    self:Dpolyline(C,false,draw_options,args.clip)
end

-- courbe d'une fonction en escalier
function luadraw_graph:Dstepfunction(def, args)
-- dessin d'une courbe d'une fonction en escalier
-- def est une table à 2 entrées permettant la définition de la fonction : { {x1,x2,x3,...,xn}, {c1,c2,...} }
--{x1,x2,...,xn} forme une subdivision de l'intervalle [x1,xn] sur lequel est dessinée la courbe
-- sur l'intervalle [x1,x2] la fonction vaut c1, sur [x2,x3] c'est c2, etc
-- args est une table à deux entrées { discont=true/false, draw_options="",clip={x1,x2,y1,y2} }
    if (def == nil) or (type(def) ~= "table") or (#def ~= 2) then return end
    args = args or {}
    local discont = args.discont
    local draw_options = args.draw_options or ""
    local C = stepfunction(def,discont)
    if C ~= nil then
        self:Dpolyline(C,false,draw_options,args.clip)
    end
end

-- courbe d'une fonction affine par morceaux
function luadraw_graph:Daffinebypiece(def, args)
-- dessin d'une courbe d'une fonction affine par morceaux
-- def est une table à 2 entrées permettant la définition de la fonction : { {x1,x2,x3,...,xn}, { {a1,b1}, {a2,b2},...} }
--{x1,x2,...,xn} forme une subdivision de l'intervalle [x1,xn] sur lequel est dessinée la courbe
-- sur l'intervalle [x1,x2] la courbe a pour équation y=a1*x+b1, sur [x2,x3] c'est y=a2*x+b2, etc
-- args est une table à deux entrées { discont=true/false, draw_options="",clip={x1,x2,y1,y2} }
    if (def == nil) or (type(def) ~= "table") or (#def ~= 2) then return end
    args = args or {}
    local discont = args.discont or true
    local draw_options = args.draw_options or ""
    local C = affinebypiece(def,discont)
    if C ~= nil then
        self:Dpolyline(C,false,draw_options,args.clip)
    end
end

function luadraw_graph:DplotXY(X,Y,draw_options,clip)
-- X et Y sont deux listes de complexes avec #X <= #Y
-- la fonction dessine la ligne polygonale constituée des points (X[k],Y[k])
    if #Y < #X then return end
    local L, z = {}
        for k,x in ipairs(X) do
            z = Z(x,Y[k])
            if z ~= nil then table.insert(L, z) end
        end
    self:Dpolyline({L},false,draw_options,clip)
end

-- solution d'équation différentielle
function luadraw_graph:Dodesolve(f,t0,Y0,args)
-- résolution par Runge Kutta 4 dans l'intervalle [t1,t2] (contenant t0) de Y'(t)=f(t,Y(t))
-- où f: (t,Y) -> f(t,Y) dans R^n avec Y(t)={y1(t),...,yn(t)} (liste de réels)
-- t0 et Y0 donnent les conditions initiales avec Y0=Y(t0),
-- la fonction appelée (odeRK4) renvoie une liste M = { {tmin,...,tmax}, {y1(tmin),..., y1(tmax)}, ..., {yn(tmin),..., yn(tmax)} }
-- args est une table à 5 entrées args = { t = {t1,t2}, out = {i1,i2}, nbdots = 50, method = "rkf45"/"rk4", draw_options = "",clip={x1,x2,y1,y2} }
-- l'argument out est une table de deux entiers {i1, i2}, les points dessinés auront pour abscisses les M[i1] et pour ordonnées les M[i2], par défaut i1=1 et i2=2
-- ce qui correspond à la fonction y1 en fonction de t
   args = args or {}
    local t = args.t or {self:Xinf(), self:Xsup()}
    local nbdots = args.nbdots or 50
    local method = args.method or "rkf45"
    local draw_options = args.draw_options or ""
    local t1, t2 = table.unpack(t)
    local out = args.out or {1,2}
    if t1 > t2 then t1, t2 = t2, t1 end
    t0 = (t0 or 0)
    if (t0 < t1) or (t0 > t2) then return end
    local C = odesolve(f,t0,Y0,t1,t2,nbdots,method)
    if C ~= nil then 
        self:DplotXY( C[out[1]], C[out[2]],draw_options,args.clip)
    end
end

function luadraw_graph:Dimplicit(f,args)
-- dessin d'une courbe implicite d'équations f(x,y)=0 dans le pavé [x1,x2]x[y1,y2]
-- args est une table à 3 entrées args = { view = {x1,x2,y1,y2}, grid = {50,50}, draw_options = "" }
-- view détermine la zone de dessin [x1,x2]x[y1,y2] et grid donne le nombre de subdivisions sur x et sur y
    args = args or {}
    local win = args.view or self.param.coordsystem
    local grid = args.grid or {50,50}
    local draw_options = args.draw_options or ""
    local x1, x2, y1, y2 = table.unpack(win)
    if x1 > x2 then x1, x2 = x2, x1 end
    if y1 > y2 then y1, y2 = y2, y1 end
    local C = implicit(f,x1,x2,y1,y2,grid)
    if C ~= nil then self:Dpolyline(C,false,draw_options) end
end

function luadraw_graph:Dcontour(f,z,args)
-- dessin de lignes de niveau de la fonction f: (x,y) -> f(x,y) à valeurs réelles
-- z est la liste des différents niveaux à tracer (équation f(x,y) = z)
-- args est une table à 4 entrées { view = {x1,x2,y1,y2}, grid = {n1,n2}, colors = {"color1", "color2", ...}, draw_options = "" }
    local a = 0
    local F = function(x,y)
            return f(x,y)-a
        end
        
    z = z or {}
    if #z == 0 then return end
    args = args or {}
    local win = args.view or self.param.coordsystem
    local grid = args.grid or {50,50}
    local draw_options = args.draw_options or ""
    local x1, x2, y1, y2 = table.unpack(win)
    if x1 > x2 then x1, x2 = x2, x1 end
    if y1 > y2 then y1, y2 = y2, y1 end
    local colors = args.colors or {}
    local nb = #colors
    local oldC = self.param.linecolor
    for i, k in ipairs(z) do
        if i <= nb then 
            self:Linecolor(colors[i])
        end
        a = k
       local C = implicit(F,x1,x2,y1,y2,grid) 
       if C ~= nil then self:Dpolyline(C,draw_options) end
    end
    self:Linecolor(oldC)
end

function luadraw_graph:Ddomain1(f,args)
-- dessine le contour de la partie du plan comprise entre la courbe de f, l'axe Ox, et les droites x=a, x=b 
-- avec f une fonction x-> f(x) réelle
-- args est une table à 5 entrées { x= {a,b}, nbdots=40, discont=true/false, nbdiv=4, draw_options="" }
   args = (args or {})
    local x = args.x or {self:Xinf(),self:Xsup()}
    local nbdots = (args.nbdots or 40)
    local discont = (args.discont or false)
    local nbdiv = (args.nbdiv or 4)
    local draw_options = args.draw_options or ""
    local a, b = table.unpack(x)
    local C = domain1(f,a,b,nbdots,discont,nbdiv)
    if C ~= nil then
        self:Dpolyline(C,true,draw_options)
    end
end

function luadraw_graph:Ddomain2(f,g,args)
-- dessine le contour de la partie du plan comprise entre la courbe de f, la courbe de g, et les droites x=a, x=b 
-- avec f une fonction x-> f(x) réelle ainsi que g
-- args est une table à 5 entrées { x= {a,b}, nbdots=40, discont=true/false, nbdiv=4, draw_options="" }
    args = (args or {})
    local x = args.x or {self:Xinf(),self:Xsup()}
    local nbdots = (args.nbdots or 40)
    local discont = (args.discont or false)
    local nbdiv = (args.nbdiv or 4)
    local draw_options = args.draw_options or ""
    local a, b = table.unpack(x)
    local C = domain2(f,g,a,b,nbdots,discont,nbdiv)
    if C ~= nil then
        self:Dpolyline(C,true,draw_options)
    end
end

function luadraw_graph:Ddomain3(f,g,args)
-- dessine le contour de la partie du plan comprise entre la courbe de f, la courbe de g, tout en se limitant dans l'intervalle [a;b]
-- avec f une fonction x-> f(x) réelle ainsi que g, supposées définies continues sur [a;b]
-- args est une table à 5 entrées { x= {a,b}, nbdots=40, discont=true/false, nbdiv=4, draw_options="" }
    args = (args or {})
    local x = args.x or {self:Xinf(),self:Xsup()}
    local nbdots = (args.nbdots or 40)
    local discont = (args.discont or false)
    local nbdiv = (args.nbdiv or 4)
    local draw_options = args.draw_options or ""
    local a, b = table.unpack(x)
    local C = domain3(f,g,a,b,nbdots,discont,nbdiv)
    if C ~= nil then
        self:Dpolyline(C,true,draw_options)
    end
end

-- segments
function luadraw_graph:Dseg(segm,scale,draw_options) -- Dseg({a,b}, scale, draw_options)
    if type(scale) == "string" then draw_options = scale; scale = 1 end
    if (segm == nil) or (type(segm) ~="table") or (#segm ~= 2) then return end
    local a, b = table.unpack(segm)
    a = toComplex(a)
    b = toComplex(b)
    scale = scale or 1
    if (a == nil) or (b == nil) then return end
    local u = b-a
    if (u.re == 0) and (u.im == 0) then return end
    if not isID(self.matrix) then
        a, b = applymatrix(a,self.matrix), applymatrix(b,self.matrix)
    end
    if scale ~= 1 then
        a,b = table.unpack(seg(a,b,scale))
    end
    local X1,X2,Y1,Y2 = table.unpack(self.param.viewport)
    local res = clipseg(a,b,X1,X2,Y1,Y2)
    if res ~= nil then -- res est une table à deux points
        local commande = self:drawcmd(draw_options)
        if cpx.dot(u,res[2]-res[1]) > 0 then -- le sens est important s'il y a une flèche
            self:Write(commande..self:Coord(res[1]))
            self:Writeln(" -- "..self:Coord(res[2])..";")
        else
            self:Write(commande..self:Coord(res[2]))
            self:Writeln(" -- "..self:Coord(res[1])..";")
        end
    end
end

function luadraw_graph:Dmarkseg(a,b,n,long,espace,angle,draw_options)
-- marque le segment [a,b] avec n petits segments penchés de angle degrés (45° par défaut), 
-- l'espacement est en unité graphique et la longueur en cm.}
    a = toComplex(a); b = toComplex(b)
    if (a == nil) or (b == nil) then return end
    n = n or 1
    long = long or 0.25
    espace = espace or 0.125
    angle = (angle or 45)*math.pi/180
    draw_options = draw_options or ""
    local l = cpx.abs(b-a)
    local v = (b-a)/l
    local u = long/2*cpx.exp(cpx.I*angle)*v 
    local c = a+(l-(n-1)*espace)*v/2
    local pas = espace*v 
    local res = {}
    for k = 1, n do 
        table.insert(res, {c-u, c+u})
        c = c + pas
    end
    self:Dpolyline(res,false,draw_options)
end

function luadraw_graph:Dmarkarc(b,a,c,r,n,long,espace,draw_options)
-- marque l'arc de cercle BAC de n segments
-- l'espacement est en unité graphique et la longueur en cm.}
    a = toComplex(a); b = toComplex(b); c = toComplex(c)
    if (a == nil) or (b == nil) or (c == nil) then return end
    r = math.abs(r or 0.5)
    n = n or 1
    long = (long or 0.25)/2
    espace = (espace or 0.0625)/r
    draw_options = draw_options or ""
    local dep = cpx.arg(b-a) + (cpx.arg((c-a)/(b-a))-(n-1)*espace)/2 
    local res = {}
    for k = 0, n-1 do 
        local p = cpx.exp(cpx.I*(dep+k*espace))
        local v = p/cpx.abs(p)
        table.insert(res, { a+r*p+long*v, a+r*p-long*v})
    end
    self:Dpolyline(res,false,draw_options)
end
    
-- polygone régulier
function luadraw_graph:Dpolyreg(sommet1, sommet2, nbcotes,sens,draw_options) -- ou (centre, sommet, nbcotes,draw_options)
    if type(sens) == "string" then draw_options = sens; sens = nil end
    local S = polyreg(sommet1, sommet2, nbcotes, sens)
    self:Dpolyline(S,true,draw_options)
end 

-- carre
function luadraw_graph:Dsquare(a,b,sens,draw_options)
    if type(sens) == "string" then draw_options = sens; sens = nil end
    local S = square(a,b,sens or 1)
    self:Dpolyline(S,true,draw_options)
end 

-- rectangle
function luadraw_graph:Drectangle(a,b,c,draw_options)
-- dessine le rectangle ayant comme sommets  consécutifs a et b (complexe) tel que le côté opposé passe par c.
    local S = rectangle(a,b,c)
    self:Dpolyline(S,true,draw_options)
end

-- parallelogram
function luadraw_graph:Dparallelogram(a,u,v,draw_options)
-- dessine le parallélogramme ayant comme sommets consécutifs a, a+u, a+u+v, a+v
    if type(a) == "table" then
        draw_options = u
        a,u,v = table.unpack(a)
    end
    local S = parallelogram(a,u,v)
    self:Dpolyline(S,true,draw_options)
end

-- suite récurrente u_(n+1)=f(u_n)
function luadraw_graph:Dsequence(f,u0,n,draw_options)
-- dessin des escaliers d'une suite récurrente
    local S = sequence(f,u0,n)
    if S ~= nil then self:Dpolyline(S,false,draw_options) end
end    

--courbe de bézier
function luadraw_graph:Dbezier(L,draw_options) -- où L = {A1,c1,c2,A2,c3,c4,A3,...}
-- dessine une série de courbes de Bézier passant par A1, A1,... et ayant comme points de contrôle c1 et c2, puis c3,c4, ...

    if (L == nil) or (type(L) ~= "table") or (#L < 3) then return end
    local a, c1, c2, b
    local i = 1
    if not isID(self.matrix) then
        L = self:Mtransform(L) -- image des points de L par la matrice de transformation courante
        if L == nil then return end
    end
    local first = true
    for k, x in ipairs(L)  do
        if i == 1 then a = x; i = 2
        else
            if i == 2 then 
                c1 = x; i = 3
            else
                if i == 3 then
                    c2 = x; i = 4
                else -- i vaut 4
                    b = x
                    -- tracé
                    if first then
                        local commande = self:drawcmd(draw_options)  -- commande draw avec les options
                        self:Write(commande..self:Coord(a))
                        first = false
                    end
                    self:Write(" .. controls "..self:Coord(c1).." and "..self:Coord(c2).." .. "..self:Coord(b))
                    i = 2
                end
            end
        end
    end
    if not first then self:Writeln(";") end
end

-- spline
function luadraw_graph:Dspline(points,v1,v2,draw_options)
-- dessine une spline passant par les points de la liste points
-- v1 et v2 sont les vecteurs tangents aux extrémités, s'ils sont égaux à nil alors c'est une spline naturelle
    if (points == nil) or (type(points) ~= "table") or (#points < 3) then return end
    self:Dpath( spline(points,v1,v2), draw_options)
end

function luadraw_graph:Dtcurve(L,options)
-- trace une courbe passant par des points donnés avec des tangentes imposées à gauche et à droite.
-- L = {pt1, {t1,a1,t2,a2}, pt2, {t1,a1,t2,a2}, ... }
-- t1 est la norme du vecteur tangent à gauche, a1 est l'angle en degré par rapport à l'horizontale du vecteur tangent à gauche
-- c'est la même chose pour le vecteur tangent à droite avec t2 et a2, mais ceux-ci sont facultatifs
-- lorsque t2 et a2 ne sont pas donnés, alors ils prennent les mêmes valeurs que t1 et a1
-- options = { showdots=false, draw_options=""}
    options = options or {}
    options.showdots = options.showdots or false
    options.draw_options = options.draw_options or ""
    if (L == nil) or (type(L) ~= "table") or (#L < 3) then return end
    local S = tcurve(L)
    if options.showdots then
        local line, dots, ctrl, k = {}, {L[1]}, {}, 0
        for _,z in ipairs(S) do
            k = k+1
            if k%4 == 0 then table.insert(dots,z) 
            elseif k%4 ~= 1 then table.insert(ctrl,z)
            end
            if z ~= "b" then table.insert(line,z) end
        end
        self:Dpolyline(line,"solid,black,line width=0.4pt")
        self:Ddots(ctrl,"color=black,scale=0.75,fill=white,mark=*")
        self:Ddots(dots,"color=black,fill=black,mark=*")
    end
    self:Dpath(S,options.draw_options)
end


-- tracer une droite d = {A,u} définie par un point A (complexe) et un vecteur directeur u (complexe non nul)
function luadraw_graph:Dline(d,B,draw_options)
--trace la droite d (si B=nil) ou bien la droite passant par les points d et B
    local A, u = nil, nil
    if (d == nil) then return end
    if type(B) == "string" then draw_options = B; B = nil end
    if (B ~= nil) then 
        B = toComplex(B)
        d = toComplex(d)
        if (B == nil) or (d == nil) then return end
        A = d
        u = B-A
    else
        if (type(d) ~= "table") or (#d ~= 2) then return end
        A = d[1]
        u = d[2]
    end
    A = toComplex(A) ; u = toComplex(u)
    if(A == nil) or (u == nil) or (u.re == 0) and (u.im == 0) then return end
    if not isID(self.matrix) then
        A, u = applymatrix(A,self.matrix), applyLmatrix(u,self.matrix)
    end
    local X1,X2,Y1,Y2 = table.unpack(self.param.viewport)
    local res = clipline({A,u},X1,X2,Y1,Y2)
    if res ~= nil then
        local commande = self:drawcmd(draw_options)
        if cpx.dot(u,res[2]-res[1]) > 0 then -- le sens est important s'il y a une flèche
            self:Write(commande..self:Coord(res[1]))
            self:Writeln(" -- "..self:Coord(res[2])..";")
        else
            self:Write(commande..self:Coord(res[2]))
            self:Writeln(" -- "..self:Coord(res[1])..";")
        end
    end
end

-- tracer une droite d par une équation cartésienne ax+by+c=0
function luadraw_graph:DlineEq(a, b, c,draw_options)
    local d = lineEq(a,b,c)
    local A = d[1]
    local u = d[2]
    A = toComplex(A)
    u = toComplex(u)
    if (A == nil) or (u == nil) or ((u.re == 0) and (u.im == 0)) then return end
    if not isID(self.matrix) then
        A, u = applymatrix(A,self.matrix), applyLmatrix(u,self.matrix)
    end
    local X1,X2,Y1,Y2 = table.unpack(self.param.viewport)
    local res = clipline({A,u},X1,X2,Y1,Y2)
    if res ~= nil then
        local commande = self:drawcmd(draw_options)
        if cpx.dot(u,res[2]-res[1]) > 0 then -- le sens est important s'il y a une flèche
            self:Write(commande..self:Coord(res[1]))
            self:Writeln(" -- "..self:Coord(res[2])..";")
        else
            self:Write(commande..self:Coord(res[2]))
            self:Writeln(" -- "..self:Coord(res[1])..";")
        end
    end
end

-- dessiner une demi-droite
function luadraw_graph:Dhline(d,B,draw_options)
--trace la demi-droite [A,A+u) si d={A,u} (si B=nil) ou bien la demi-droite [d,B) si B non nil
    local A, u = nil, nil
    if (d == nil) then return end
    if type(B) == "string" then draw_options = B; B = nil end
    if (B ~= nil) then 
        B = toComplex(B)
        d = toComplex(d)
        if (B == nil) or (d == nil) then return end
        A = d
        u = B-A
    else
        if (type(d) ~= "table") or (#d ~= 2) then return end
        A = d[1]
        u = d[2]
    end
    A = toComplex(A) ; u = toComplex(u)
    if(A == nil) or (u == nil) or (u.re == 0) and (u.im == 0) then return end        
    if not isID(self.matrix) then
        A, u = applymatrix(A,self.matrix), applyLmatrix(u,self.matrix)
    end
    local X1,X2,Y1,Y2 = table.unpack(self.param.viewport)
    local M = self.matrix  
    self.matrix = ID --la transformation a déjà été faite
    if (A.re < X1) or (A.re > X2) or (A.im < Y1) or (A.im > Y2) then -- A est hors de la vue courante
        self:Dline(A,A+u,draw_options) -- on dessine la droite 
    else -- A est dans la fenêtre
        B = A+((X2-X1)+(Y2-Y1))*u -- on met B est la droite {A,u} mais hors de la vue courante
        self:Dseg({A,B},1,draw_options) -- on dessine le segment [A,B]
    end
    self.matrix = M -- restitution
end

function luadraw_graph:Dperp(d,A,draw_options)
-- dessine la perpendiculaire à la d passant par A
-- d est soit une droite (un point et un vecteur directeur), soir un vecteur non nul
    self:Dline(perp(d,A))
end

function luadraw_graph:Dparallel(d,A,draw_options)
-- dessine la parallèle à la d passant par A
-- d est soit une droite (un point et un vecteur directeur), soir un vecteur non nul
    self:Dline(parallel(d,A),draw_options)
end

function luadraw_graph:Dmed(A,B,draw_options) -- ou Dmed(seg,draw_options)
-- dessine la médiatrice du segment seg ou [A;B]
    if (type(B) == "string") then draw_options = B; B = nil end
    self:Dline(med(A,B),draw_options)
end

function luadraw_graph:Dbissec(B,A,C,interior,draw_options)
-- dessine une bissectrice de l'angle géométrique BAC
    if (type(interior) == "string") then draw_options = interiorB; interior = nil end
    self:Dline(bissec(B,A,C,interior),draw_options)
end

function luadraw_graph:Dtangent(p,t0,long,draw_options)
-- dessin de la tangente à la courbe paramétrée par t->p(t) (à valeurs complexes)
-- au point de paramètre t0. 
-- si le paramètre long est égal à nil, on trace toute la droite, sinon, un segment de longueur long
    if type(long) == "string" then draw_options = long; long = nil end
    local S = tangent(p,t0,long)
    if long == nil then self:Dline(S,draw_options)
    else self:Dseg(S,1,draw_options)
    end
end

function luadraw_graph:DtangentC(f,x0,long,draw_options)
-- dessin de la tangente à la courbe cartésienne d'équation y=f(x)
-- au point d'abscisse x0
-- si le paramètre long est égal à nil, on trace toute la droite, sinon, un segment de longueur long
    if type(long) == "string" then draw_options = long; long = nil end
    local S = tangentC(f,x0,long)
    if long == nil then self:Dline(S,draw_options)
    else self:Dseg(S,1,draw_options)
    end
end

function luadraw_graph:DtangentI(f,x0,y0,long,draw_options)
-- dessin de la tangente à la courbe implicite d'équation f(x,y)=0
-- au point (x0,y0) supposé sur la courbe
-- si le paramètre long est égal à nil, on trace toute la droite, sinon, un segment de longueur long
    if type(long) == "string" then draw_options = long; long = nil end
    local S = tangentI(f,x0,y0,long)
    if long == nil then self:Dline(S,draw_options)
    else self:Dseg(S,1,draw_options)
    end
end

function luadraw_graph:Dtangent_from(A,p,t1,t2,dp,draw_options,out)
-- dessin des tangentes à la courbe paramétrée par p:t ->p(t), issues du point from A (nombre complexe)
-- t1,t2 :bornes d el'intervalle de recherche
-- dp (optionnel) fonction dérivée de p
-- out doit être une table, elle permet de récupérer les points de contacts
    if type(dp) == "string" then out = draw_options; draw_options = dp; dp = nil end
    out = out or {}
    local S = tangent_from(A,p,t1,t2,dp)
    for _,B in ipairs(S) do
        table.insert(out,B)
        self:Dline(A,B,draw_options)
    end
end

function luadraw_graph:Dnormal(p,t0,long,draw_options)
-- dessin de la normale à la courbe paramétrée par t->p(t) (à valeurs complexes)
-- au point de paramètre t0. 
-- si le paramètre long est égal à nil, on trace toute la droite, sinon, un segment de longueur long
    if type(long) == "string" then draw_options = long; long = nil end
    local S = normal(p,t0,long)
    if long == nil then self:Dline(S,draw_options)
    else self:Dseg(S,1,draw_options)
    end
end

function luadraw_graph:DnormalC(f,x0,long,draw_options)
-- dessin de la normale à la courbe cartésienne d'équation y=f(x)
-- au point d'abscisse x0
-- si le paramètre long est égal à nil, on trace toute la droite, sinon, un segment de longueur long
    if type(long) == "string" then draw_options = long; long = nil end
    local S = normalC(f,x0,long)
    if long == nil then self:Dline(S,draw_options)
    else self:Dseg(S,1,draw_options)
    end
end

function luadraw_graph:DnormalI(f,x0,y0,long,draw_options)
-- dessin de la normale à la courbe implicite d'équation f(x,y)=0
-- au point (x0,y0) supposé sur la courbe
-- si le paramètre long est égal à nil, on trace toute la droite, sinon, un segment de longueur long
    if type(long) == "string" then draw_options = long; long = nil end
    local S = normalI(f,x0,y0,long)
    if long == nil then self:Dline(S,draw_options)
    else self:Dseg(S,1,draw_options)
    end
end

----- Labels -----------------------------------------------------
function luadraw_graph:Dlabel(...) -- Dlabel(texte,anchor,options, texte,anchor,options, ... )
-- dessiner un label, anchor est un complexe (point), 
-- args est une table à 3 entrées { pos = nil, dist = 0, dir=nil, node_options = "" }
-- pos est "center" ou "N", "NE", "NE", "SE", "S", "SW", "W" ou "NW"
-- dist est la distance en cm du texte par rapport au node, si dist = nil c'est 0 par défaut
-- dir={dirX,dirY,dep} est la direction de l'écriture (nil pour le sens par défaut)
-- node_options est une chaîne passée directement à tikz, ex: "rotate=45, draw, fill=red" (options locales)
   local style = { ["N"] = "above", ["NE"] = "above right", ["NW"] = "above left",
                    ["S"] = "below", ["SE"] = "below right", ["SW"] = "below left",
                    ["W"] = "left", ["E"] = "right" }
    local options, first, sep = "", true, ""
    if self.param.labelcolor ~= "" then options = "color="..self.param.labelcolor; sep = "," end
    local commande = "\\draw[-"..sep..options.."]"
    local texte, anchor, pos, dist, dir = "", nil, self.param.labelstyle, 0, self.param.labeldir
    local node_options = ""
    local args = {}
    local x1,x2,y1,y2 = table.unpack(self:Getview())
    local pt = 0.4/mm -- épaisseur de 0.4pt exprimée en cm
    local epX, epY = pt/self.Xscale, pt/self.Yscale
    
    local alabel=function() -- dessiner un label
        if (texte == nil) or (texte == "") then return end
        if self.param.labelsize ~= "" then texte = "\\"..self.param.labelsize.." "..texte end
        pos = args.pos or pos
        dist = args.dist or dist
        dir = args.dir or dir
        node_options = args.node_options or node_options
        anchor = toComplex(anchor)
        if (anchor == nil) then return end
        local dir_str = ""
        if (type(dir) == "table") and (#dir > 0) then -- changement de direction de l'écriture
            local u, v, dep = table.unpack(dir)
            u = toComplex(u); 
            if v == nil then v = cpx.I*u else v = toComplex(v) end
            if dep == nil then dep = 0 end
            dep = toComplex(dep)
            if not isID(self.matrix) then
                u, v = table.unpack( self:MLtransform({u,v}) )
            end
            u, v = cpx.normalize(u), cpx.normalize(v)
            dir_str = "cm={"..strReal(u.re)..","..strReal(u.im)..","..strReal(v.re)..","..strReal(v.im)..",("..strReal(dep.re)..","..strReal(dep.im)..")}"
        end
        local tikz_pos = style[pos]
        sep = ""
        if tikz_pos ~= nil then sep = "," else tikz_pos = "" end
        if (dist > 0) and (tikz_pos ~= "") then tikz_pos = tikz_pos.."="..dist.."cm" end
        if not isID(self.matrix) then
            anchor = applymatrix(anchor,self.matrix)
        end
        if (anchor.re < x1-epX) or (anchor.re > x2+epX) or (anchor.im < y1-epY) or (anchor.im > y2+epY) then return end -- point en dehors de la vue courante
        if (self.param.labelangle ~= 0) then tikz_pos = tikz_pos..sep.."rotate="..self.param.labelangle end
        if first then self:Write(commande); first = false end
        local opt_str = tikz_pos
        if (node_options == "") or (tikz_pos == "") then sep = "" else sep = "," end
        opt_str = opt_str..sep..node_options
        if (opt_str == "") or (dir_str == "") then sep = "" else sep = "," end
        opt_str = opt_str..sep..dir_str
        self:Write(" "..self:Coord(anchor).." node["..opt_str.."] {"..texte.."}")
    end
    for k, aux in ipairs{...} do
        if k%3 == 1 then texte = aux
        else
            if k%3 == 2 then anchor = aux
            else  args = aux; alabel()
            end
        end
    end
    if not first then self:Writeln(";") end
end


------ dots  -------------------------------------------------------
function luadraw_graph:Ddots(L, mark_options)
-- L doit une liste de complexes représentant les points à dessiner (ou liste de listes)
-- mark_options est une chaîne d'options définissant marker options dans tikz
-- exemple "color=green, line width=1.2, scale=0.25"
    L = (L or {})
    if (type(L) == "number") or isComplex(L) then L = {{L}} end
    if (type(L) ~= "table") or (#L == 0) then return end
    if (type(L[1]) == "number") or isComplex(L[1]) then L = {L} end
    mark_options = (mark_options or "")
    local scale = self.param.dotscale
    local r = (self.param.dotsize.re + self.param.dotsize.im*self.param.linewidth/10)/2
    r = r*scale
    local first = true
    local commande = "\\draw[mark="..self.param.dotstyle..",mark size="..r.."pt"
    if mark_options ~= "" then commande = commande..",mark options={"..mark_options.."}" end
    
    local drawdot = function(dots)
        if first then self:Write(commande.."]"); first = false end
        for _,z in ipairs(dots) do
            self:Write(" plot coordinates {"..self:Coord(z).."}")
        end
    end   

    for _, aux in ipairs(L) do
        local len = #aux
        if len > 0 then
            if not isID(self.matrix) then
                aux = self:Mtransform(aux)  --application de la matrice courante
            end
            local X1,X2,Y1,Y2 = table.unpack(self.param.viewport)
            aux = clipdots(aux,X1,X2,Y1,Y2) -- élimination des "trop grands" et conversion reel -> complexe, on récupère une liste de listes
            for _,cp in ipairs(aux) do 
                drawdot(cp) 
            end
        end
    end
    if not first then self:Writeln(";") end
end

function luadraw_graph:Dlabeldot(texte,anchor,args)
-- affiche un texte et le point d'ancrage (dans le style courant), args est identique au cas de Dlabel plus le champ mark_options
    anchor = toComplex(anchor)
    args =  args or {}
    local mark_options = args.mark_options or ""
    self:Dlabel(texte,anchor,args)
    if mark_options == "" then mark_options = "color="..self.param.linecolor
    else mark_options = "color="..self.param.linecolor..","..mark_options
    end
    self:Ddots(anchor,mark_options)
end

----- angle droit, arc, secteur ------------------------------------------------------
function luadraw_graph:Dangle(B,A,C,r,draw_options)
    A, B, C = toComplex(A), toComplex(B), toComplex(C)
    if (A == nil) or (B == nil) or (C == nil) then return end
    if type(r) == "string" then draw_optiopns = r; r = 0.25 end
    local D, E, F = angleD(B,A,C,r)
    if (D == nil) or (E == nil) or (F == nil) then return end
    if self.param.fillstyle ~= "none" then
        local oldL = self.param.linestyle
        local oldF = self.param.fillstyle
        self:Linestyle("noline")
        self:Dpolyline({{D,E,F,A}},true,draw_options)
        self:Linestyle(oldL)
        self.param.fillstyle = "none"
        self:Dpolyline({{D,E,F}},false,draw_options)
        self.param.fillstyle = oldF
    else self:Dpolyline({{D,E,F}},false,draw_options)
    end
end

--------- Dessin de chemins --------------------------------------------

-- path
function luadraw_graph:Dpath(L,draw_options,clip) 
-- dessine le chemin contenu dans L, L est une table de complexes et d'instructions
-- ex: Dpath( {-1,2+i,3,"l", 4, "m", -2*i,-3-3*i,"l","cl",...} )
-- "m" pour moveto, "l" pour lineto, "b" pour bézier, "c" pour cercle, "ca" pour arc de cercle, "ea" arc d'ellipse, "e" pour ellipse, "s" pour spline naturelle, "cl" pour close
-- "la" pour line arc (ligne aux coins arrondis), "cla" ligne fermée aux coins arrondis
    clip = clip or false -- indique si  L est un chemin de clipping
    if (L == nil) or (type(L) ~= "table") or (#L < 3) then return end
    draw_options = draw_options or ""
    local commande
    if clip then commande = "\\clip "
    else commande = self:drawcmd(draw_options,clip)
    end
    if commande == nil then return end
    local debut = true
    local res = {} -- résultat
    local crt = {} -- composante courante
    local aux = {} -- lecture en cours
    local last, first = nil, nil -- dernier lu et premier à venir
    local traiter
    local Mcoord = function(z) return self:Coord(z) end
    
    local Tcoord = function(z) -- applique la matrice courante à z
        return self:Coord(applymatrix(z,self.matrix))
    end
    
    if not isID(self.matrix) then Mcoord = Tcoord end -- transformation des points par la matrice courante
    
    local lineto = function() -- traitement du lineto
        -- on relie les points par une ligne
        if debut then self:Write(commande); debut = false end
        if first ~= nil then self:Write(" -- ") end
        self:Write(Mcoord(aux[1]))
        for k = 2, #aux do
            self:Write(" -- "..Mcoord(aux[k]))
        end
        first = last
        aux = {}
    end
    
    local moveto = function() -- traitement du moveto
    -- on démarre une nouvelle composante
            if not debut then self:Write(" ") end
            first = nil
            aux = {last}
    end
    
    local close = function() -- traitement du closepath
        -- en principe il y a eu une instruction avant autre que move, aux doit être vide et pas crt
        self:Write("--cycle")
        aux = {}
    end
    
    local Bezier = function()
        -- aux contient une ou plusieurs courbes de bézier
        local i
        if debut then self:Write(commande); debut = false end
        if first == nil then i = 1 else i = 2 end
        for _, z in ipairs(aux) do
            if i == 1 then self:Write(Mcoord(z)); i = 2
            else
                if i == 2 then self:Write(" .. controls "..Mcoord(z)); i = 3
                else
                    if i == 3 then self:Write(" and "..Mcoord(z)); i = 4
                    else    
                        if i == 4 then self:Write(" .. "..Mcoord(z)); i = 5 
                        else
                            if i == 5 then i = 2 end -- on est sur le caractère "b"
                        end
                    end
                end
            end
        end
        first = last
        aux = {}
    end
    
    local Spline = function ()
            if first ~= nil then 
            table.insert(aux,1,first)
        end
        aux = spline(aux) -- spline naturelle
        if aux ~= nil then
            if first ~= nil then table.remove(aux,1) end -- le premier point est déjà exporté
            Bezier()
        end
        aux = {}
    end
    
    local Circle = function()
    -- il faut un point et le centre
        if first ~= nil then 
            table.insert(aux,1,first)
        end
        local a, c, r = aux[1], nil, nil
        if #aux == 2 then -- on a un point et le centre
            c = aux[2]; r = cpx.abs(a-c)
        else
            if #aux == 3 then -- on a trois points du cercle
                c = interD(med(a,aux[2]), med(a,aux[3]))
                if c == nil then aux = {}; return end
                r = cpx.abs(a - c)
            else aux = {}; return
            end
        end
        aux = arcb(a,c,a,r,1)
        if aux ~= nil then
            if first ~= nil then table.remove(aux,1) end -- le premier point est déjà exporté
            Bezier()
        end
        aux = {}
    end
    
    local Arc = function()
        local n = #aux
        if (n < 3) or (n > 5) then aux = {}; return end
        if first ~= nil then 
            table.insert(aux,1,first)
        end
        aux = arcb(table.unpack(aux))
        if aux ~= nil then
            if first ~= nil then self:Write(" -- "); first = nil end -- pour relier le premier point de l'arc au précédent
            local newfirst = aux[#aux-1]
            Bezier()
            first = newfirst -- dernier point de l'arc
        end
        aux = {}
    end
    
    local Earc = function() -- ellipticarc(b,a,c,rx,ry,sens,inclin)
        local n = #aux
        if (n < 4) or (n > 7) then aux = {}; return end
        if first ~= nil then 
            table.insert(aux,1,first)
        end
        aux = ellipticarcb(table.unpack(aux))
        if aux ~= nil then
            if first ~= nil then self:Write(" -- "); first = nil end -- pour relier le premier point de l'arc au précédent
            local newfirst = aux[#aux-1]
            Bezier()
            first = newfirst -- dernier point de l'arc
        end
        aux = {}
    end    
    
    local Ellipse = function() -- ellipse(p,c,rx,ry,sens,inclin)
        local n = #aux
        if (n < 3) or (n > 5) then aux = {}; return end
        if first ~= nil then 
            table.insert(aux,1,first)
        end
        local p, c, rx, ry, inclin = table.unpack(aux)
        aux = ellipticarcb(p,c,p,rx,ry,1,inclin)
        if aux ~= nil then
            if first ~= nil then self:Write(" -- "); first = nil end -- pour relier le premier point de l'arc au précédent
            local newfirst = aux[#aux-1]
            Bezier()
            first = newfirst -- dernier point de l'arc
        end
        aux = {}
    end 
    local Rline = function(close) --on appelle roundline(L,r)
        local n = #aux
        if (n < 2) then aux = {}; return end
        if first ~= nil then 
            table.insert(aux,1,first)
        end
        local r = table.remove(aux)
        if (type(r) ~= "number") or (r <= 0) then aux = {}; return end
        local C = roundline(aux,r,close,true)
        if C ~= nil then
            if first ~= nil then 
                if not close then table.remove(C,1) -- le premier point est déjà exporté
                end
            end 
            aux = {}
            for _,z in ipairs(C) do
                if (type(z) == "number") or isComplex(z) then table.insert(aux,z); last = z 
                else
                    if type(z) == "string" then traiter[z]() end
                end            
            end
        end
        aux = {}
    end
    
    local cRline = function()
        Rline(true)
    end
-- corps de la fonction dpath
    local clippee
    local aux2 = path(L)
    if not isID(self.matrix) then aux2 = self:Mtransform(aux2) end
    local X1,X2,Y1,Y2 = table.unpack(self.param.viewport)
    clippee = (not clip) and needclip(aux2,X1,X2,Y1,Y2)
    if clippee and self.bbox then
        self:Writeln("\\begin{scope}")
        self:Writeln("\\clip "..self:strCoord(X1,Y1).." rectangle "..self:strCoord(X2,Y2)..";")
    end
    traiter = { ["s"]=Spline, ["l"]=lineto, ["m"]=moveto, ["cl"]=close, ["b"]=Bezier, ["c"]=Circle, ["ca"]=Arc, ["ea"]=Earc, ["e"]=Ellipse, ["la"]=Rline, ["cla"]=cRline } 
    for _, z in ipairs(L) do
        if (type(z) == "number") or isComplex(z) then table.insert(aux,z); last = z 
        else
            if type(z) == "string" then traiter[z]() end
        end
    end
    if not debut then self:Writeln(";") end
    if clippee and self.bbox then self:Writeln("\\end{scope}") end
end

-- clipping avec un chemin
function luadraw_graph:Beginclip(p,inverse) -- p = path
    inverse = inverse or false
    self:Writeln("\\begin{scope}")
    if inverse then
        local chem = self:Box2d()
        --local chem = reverse(self:Box2d())
        local L = path(p)[1]
        local G = isobar(L)
        local A, B = L[1], L[2]
        if cpx.det(A-B,B-G) >= 0 then chem = reverse(chem) end
        insert(chem,{"l","cl"})
        table.insert(p,2,"m")
        self:Dpath( concat(chem,p),"",true) -- path doit être dans le sens trigonométrique
    else self:Dpath(p,"",true)
    end
end

function luadraw_graph:Endclip()
    self:Writeln("\\end{scope}")
end

-- arc de cercle
function luadraw_graph:Darc(B,A,C,r,sens,draw_options)
-- dessine un arc de cercle de centre A, allant de B à C avec un rayon de r en cm
-- sens = 1 pour sens trigo ou -1 sinon
    local S = arcb(B,A,C,r,sens or 1) -- arc en courbes de Bézier
    if S == nil then return end
    if self.param.fillstyle ~= "none" then
        local oldL = self.param.linestyle
        local oldF = self.param.fillstyle
        self:Linestyle("noline")
        self:Dpath(concat(S,{A,"l","cl"}),draw_options)
        self:Linestyle(oldL)
        self.param.fillstyle = "none"
        self:Dpath(S,draw_options)
        self.param.fillstyle = oldF
    else self:Dpath(S,draw_options)
    end
end

function luadraw_graph:Dwedge(B,A,C,r,sens,draw_options)
-- dessine un secteur angulaire
    self:Dpath( {B,A,C,r,sens,"ca",A,"l","cl"}, draw_options)
end

--cercle
function luadraw_graph:Dcircle(c,r,d,draw_options) -- ou Dcircle({c,r,d},draw_options)
-- dessine le cercle de centre c et de rayon r (si d=nil) ou bien passant par les points c, r et d
    if (type(c) == "table") and (not isComplex(c)) then
        if type(r) == "string" then draw_options = r end
        c,r,d = table.unpack(c)
    else
        if type(d) == "string" then draw_options = d; d = nil end
    end
    local S = circleb(c,r,d)
    self:Dpath(S,draw_options) -- cercle en courbes de Bézier
end

--ellipse
function luadraw_graph:Dellipse(c,rx,ry,inclin,draw_options)
-- renvoie les points de l'ellipse de centre c et de rayons rx et ry, inclin est l'inclinaison en degrés par rapport à l'horizontale (0 par défaut)
   local S = ellipseb(c,rx,ry,inclin)
   self:Dpath(S,draw_options) -- ellipse en courbes de Bézier
end

-- arc d'ellipse
function luadraw_graph:Dellipticarc(B, A, C, rx, ry, sens, inclin,draw_options)
-- dessine un arc d'ellipse de centre A, et de AB vers AC
-- rx et ry sont en cm
-- sens = +/-1 (1 pour le sens trigo), inclin est l'inclinaison en degrés par rapport à l'horizontale
    if type(inclin) == "string" then draw_options = inclin; inclin = nil end
    local S = ellipticarcb(B, A, C, rx, ry, sens or 1, inclin or 0)
    self:Dpath(S,draw_options) -- arc d'ellipse en courbes de Bézier
end   

-- gestion des couleurs ------------------------------------------------

require("luadraw_colors.lua")

function luadraw_graph:Newcolor(name, color)
-- definit dans l'export tikz une nouvelle couleur
-- name est le nom (chaîne)
-- color est une table de trois composantes: rouge, vert, bleu (entre 0 et 1) ou bien une chaîne définissant une couleur
    if type(color) == "table" then
        self:Writeln("\\definecolor{"..name.."}{rgb}{"..strReal(color[1])..","..strReal(color[2])..","..strReal(color[3]).."}")
    elseif type(color) == "string" then
        self:Writeln("\\colorlet{"..name.."}{"..color.."}")
    end
end

------------- import an image --------------------------
function luadraw_graph:Dimage(file,anchor,options)
-- file = string (full name of the image file)
-- anchor = complex number
-- options = {pos="center", matrix=nil, name="", graphics_options=""}
    local style = { ["N"] = "above", ["NE"] = "above right", ["NW"] = "above left",
                    ["S"] = "below", ["SE"] = "below right", ["SW"] = "below left",
                    ["W"] = "left", ["E"] = "right" }
    options = options or {}
    anchor = toComplex(anchor)
    anchor = applymatrix(anchor,self.matrix)
    local pos = options.pos or "center"
    pos = style[pos]
    local name = options.name or ""
    if name ~= "" then name = "("..name..") " end
    local graphics_options = options.graphics_options or ""
    local mat 
    if isID(self.matrix) then
        mat = options.matrix 
        if mat == nil then mat = ID end
    else
        mat = {Z(0,0), self.matrix[2], self.matrix[3]}
        if options.matrix ~=nil then mat = composematrix(mat, options.matrix) end
    end
    if not isID(mat) then
        local t,u,v = table.unpack( map(toComplex,mat) )
        t = Z(t.re*self.Xscale, t.im*self.Yscale)
        u = Z(u.re*self.Xscale, u.im*self.Xscale)
        v = Z(v.re*self.Xscale, v.im*self.Yscale)
        mat = ",cm={"..strReal(u.re)..","..strReal(u.im)..","..strReal(v.re)..","..strReal(v.im)..",("..strReal(t.re)..","..strReal(t.im)..")}"
    else mat = ""
    end
    if pos ~= nil then pos = ","..pos else pos = "" end
    local cmd = "\\node[line width=0.3pt,inner sep=-0.15pt"..mat..pos.."] "..name.."at ".. self:Coord(anchor).."{\\includegraphics["..graphics_options.."]{"..file.."}};"
    self:Writeln(cmd)
end

function luadraw_graph:Dmapimage(file, parallelo, options)
-- file = string (name of the image)
-- parallelogram = {vertice, vector1, vector2} 
    options = options or {}
    local name = options.name or ""
    local clip = options.clip or false
    local border = options.border_options or nil -- draw_options to draw the border
    local a, u, v = table.unpack( parallelo )
    if clip then self:Beginclip( {a, a+u,a+u+v,a+v,"l","cl"} ) end
        self:Dimage(file, Z(0,0),{pos="NE", name=name, matrix={a,u,v}, 
            graphics_options="width=1cm,height=1cm"})
    if clip then self:Endclip() end
    if border ~= nil then
        self:Dpolyline(parallelogram(a,u,v), true, border)
    end
end

return luadraw_graph
