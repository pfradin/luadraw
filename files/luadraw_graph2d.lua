-- luadraw_graph2d.lua
-- date 2025/11/13
-- version 2.3
-- Copyright 2025 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.

-- ce module ajoute le tracé d'axes divers ou de grilles au module luadraw_graph

local luadraw_graph2d = require "luadraw_graph"

maxGrad = 100 -- nombre max de graduations
defaultlabelshift = 0.1875
defaultxylabelsep = 0
defaultlegendsep = 0.2
dollar = true -- pour les labels des graduations, ajout ou on des dollars

function simplifyFrac(a,b)
-- renvoie la fraction d'entiers a/b simplifiée
    local sg = 1
    if ((a < 0) and (b > 0)) or ((a > 0) and (b < 0)) then sg = -1 end
    a, b = math.abs(a), math.abs(b)
    if (math.floor(a) == a) and (math.floor(b) == b) then 
        local d = gcd(a,b)
        return sg*a//d, b//d
    else return sg*a, b
    end
end

function addFrac(a,b,c,d)
-- renvoie la somme a/b + c/d sous forme de fraction
-- a, b, c, d sont supposés être des entiers avec b et d non nuls.
    local num, den = a*d+b*c, b*d
    return simplifyFrac(num, den)
end

function gradLabel(a,b,text)
-- mise en forme d'un label pour une graduation : a*text/b, renvoie une chaîne
-- dollar = true/false indique s'il faut des dollars ou non
    function label(x)
            local str
            if type(x) == "number" then str = num(x) else str = x end
            if dollar then return "$"..str.."$"
            else return str
            end
        end
    if a == 0 then return label(0) -- \fraction nulle
    else
        if text == "" then -- pas de text
            if b == 1 then return label(a)
            else
                if a > 0 then
                    if dollar then return "$\\frac{".. num(a).."}{"..num(b).."}$" 
                    else return num(a).."/"..num(b) 
                    end 
                else -- a < 0
                    if dollar then return "$-\\frac{".. num(-a).."}{"..num(b).."}$" 
                    else return num(a).."/"..num(b) 
                    end
                end
            end
        else -- text non vide
            if b == 1 then
                        if a == -1 then return label("-"..text)
                        else 
                            if  a == 1 then return label(text)
                            else return label(num(a)..text)
                            end
                        end
            else -- b différent de 1 
                if  a > 0 then 
                        if dollar then 
                            if a ~= 1 then return "$\\frac{"..num(a)..text.."}{"..num(b).."}$"
                            else return "$\\frac{"..text.."}{"..num(b).."}$"
                            end
                        else 
                            if a ~= 1 then return num(a)..text.."/"..num(b)
                            else return text.."/"..num(b)
                            end
                        end 
                else  -- a < 0    
                        if dollar then 
                            if a ~= -1 then return "$-\\frac{"..num(-a)..text.."}{"..num(b).."}$"
                            else return "$-\\frac{"..text.."}{"..num(b).."}$"
                            end
                        else 
                            if a ~= -1 then return num(a)..text.."/"..num(b)
                            else return "-"..text.."/"..num(b)
                            end
                        end 
                end
            end
        end
    end
end

function luadraw_graph2d:Poslab(dir,alpha)
-- renvoie la position du label quand celui-ci est placé au bout du vecteur dir
-- en faisant un angle de alpha degrés par rapport à Ox
    dir = toComplex(dir)
    dir = applyLmatrix(dir,self.matrix)
    local angle = self:Arg(dir)*180/math.pi - alpha -- angle entre -180 et 180
    if angle > 180 then angle = angle -360 end
    if angle < -180 then angle = angle + 360 end
    if (-22.5 < angle) and (angle <= 22.5) then return "E" end
    if (22.5 < angle) and (angle <= 67.5) then return "NE" end
    if (67.5 < angle) and (angle <= 112.5) then return "N" end
    if (112.5 < angle) and (angle <= 157.5) then return "NW" end
    if (157.5 < angle) or(angle <= -157.5) then return "W" end
    if (-157.5 < angle) and (angle <= -112.5) then return "SW" end
    if (-112.5 < angle) and (angle <= -67.5) then return "S" end
    if (-67.5 < angle) and (angle <= -22.5) then return "SE" end
end

function luadraw_graph2d:Dgradline(d, options)
-- dessin d'un axe gradué avec d = {A,u}
    options = options or {}
    local showaxe = options.showaxe or 1 --affichage ou non de l'axe (1 ou 0)
    local arrows = options.arrows or "-" -- pas de flèche par défaut
    local limits = options.limits or "auto" -- {N1,N2} intervalle  représentant le segment [A+N1*u, A+N2*u], "auto" par défaut pour toute la droite entière
    local unit = options.unit or 1 -- graduation de 1 en 1 par défaut
    local gradlimits = options.gradlimits or "auto" -- {N1,N2} intervalle des graduations (entières), égal à limits par défaut
    local nbsubdiv = options.nbsubdiv or 0 -- nombre de subdivisions par unité

    local tickpos = options.tickpos or 0.5 -- nombre entre 0 et 1
    local tickdir = options.tickdir or "auto" -- direction graduations (ortho par défaut)
    local xyticks = options.xyticks or 0.2 -- longueur des graduations
    local xylabelsep = options.xylabelsep or defaultxylabelsep -- distance labels-graduations

    local originpos = options.originpos or "center" -- "none" or "center" or "left" or "right"
    local originnum = options.originnum or 0 -- les labels sont: (originnum + unit*n)"labeltext"/labelden
    
    local legend = options.legend or "" -- légende
    local legendpos = options.legendpos or 0.975 -- nombre entre 0 et 1
    local legendsep = options.legendsep or defaultlegendsep 
    local legendangle = options.legendangle or "auto"  -- "auto" pour angle automatique
    local legendstyle = options.legendstyle or "auto" -- "auto" "top" ou "bottom" légende au dessus par défaut

    local labelpos = options.labelpos or "bottom" -- "none" or "top" or "bottom"
    local labelden = options.labelden or 1 -- dénominateur (entier)
    local labeltext = options.labeltext or "" -- texte ajouté aux labels, vide par défaut
    local labelstyle = options.labelstyle or "auto" -- "auto"  or "E" or "W",...
    local labelangle = options.labelangle or 0 -- angle des labels en degrés par rapport à l'horizontale
    local labelcolor = options.labelcolor or ""    
    local labelshift = options.labelshift or 0 -- décalage systématique des labels
    local nbdeci = options.nbdeci or 2 -- nb de décimales, 2 par défaut
    local numericFormat = options.numericFormat or 0 -- format d'affichage
    local mylabels = options.mylabels or "" -- labels personnels, liste {pos1,texte1, pos2,texte2,...} ATTENTION : pos est l'abscisse sur l'axe (A,u)
    
    local A, u = nil, nil
    if (d == nil) or (type(d) ~= "table") or (#d ~= 2) then return end
    A = d[1]
    u = d[2]
    A = toComplex(A) ; u = toComplex(u)
    if (A == nil) or (u == nil) then return end
    A = applymatrix(A,self.matrix)
    if tickdir ~= "auto" then tickdir = applyLmatrix(tickdir,self.matrix) end
    local v = applyLmatrix(cpx.I*u,self.matrix)
    u = applyLmatrix(u,self.matrix)
    if cpx.det(u,v) < 0 then v = -v end
    nbsubdiv = math.floor(math.abs(nbsubdiv))
    local pas, L = u/(nbsubdiv+1)
    if limits == "auto" then
        local ep = 0.01 -- pour élargir la fenêtre de clipping
        local X1,X2,Y1,Y2 = table.unpack(self.param.viewport)
        L = clipline({A,u},X1-ep/self.Xscale,X2+ep/self.Xscale,Y1-ep/self.Yscale,Y2+ep/self.Yscale)
    else 
        L = { A+limits[1]*u, A+limits[2]*u }
    end
    if L == nil then return end
    self:Savematrix() ; self:IDmatrix()
    if cpx.dot(u, L[2]-L[1]) <= 0 then L = { L[2], L[1] } end
    local k1 = cpx.abs((L[1]-A)/pas)
    local k2 = cpx.abs((L[2]-A)/pas)
    if cpx.dot(u,L[2]-A) < 0 then k2 = -k2; k1 = -k1
    else
        if cpx.dot(u,L[1]-A) < 0 then k1 = -k1 end
    end
    k1 = math.ceil(k1);  k2 = math.floor(k2)
    if gradlimits ~= "auto" then 
        local q1 = math.floor(gradlimits[1])*(nbsubdiv+1)
        local q2 = math.floor(gradlimits[2])*(nbsubdiv+1)
        if q2 < q1 then q1, q2 = q2, q1 end
        if k1 < q1 then k1 = q1 end
        if q2 < k2 then k2 = q2 end
    end
    if (k2-k1+1)/(1+nbsubdiv) > maxGrad then k1 = 0; k2 = 0 end --trop de graduations principales
    if k1*k2 > 0 then originpos = "none" end --origine non visible car hors segment
    local n
    if tickdir == "auto" then n = v 
    else 
        n = tickdir
        if n == 0 then n = v end
        if cpx.dot(n,u) < 0 then n = -n end
    end
    local n1 = n/self:Abs(n)*xyticks*tickpos
    local n2 = n/self:Abs(n)*xyticks*(1-tickpos)
    -- axe et légende
    if showaxe == 1 then self:Dseg(L,1,arrows) end -- dessin de l'axe
    --local oldarrows = self.param.arrows
    local oldstyle = self.param.linestyle
    local oldlabelangle = self.param.labelangle
    local oldlabelcolor = self.param.labelcolor
    self:Linestyle("solid") --; self:Arrows("-")
    if legend ~= "" then -- 
        if legendangle == "auto" then
                if labelstyle == "auto" 
                    then legendangle = self:Arg(u)*180/math.pi
                         if legendangle >= 90 then legendangle = legendangle - 180 end
                         if legendangle <= -90 then legendangle = legendangle + 180 end
                else legendangle = 0 
                end
        end
        local ldir
        if legendpos == 0 then 
            ldir = -u
        else
            if legendpos == 1 then 
                ldir = u
            else
                if labelpos == "bottom" 
                then 
                    ldir = n
                else 
                    ldir = -n
                end
            end
        end
        if legendstyle == "auto" then legendstyle = self:Poslab(ldir,legendangle) end
        local lpos = getdot(legendpos,L)+legendsep*ldir/self:Abs(ldir) -- position de la légende
        if legendangle ~= 0 then
            self:Dlabel(legend,lpos, {pos = legendstyle, node_options="rotate="..legendangle})
        else
            self:Dlabel(legend,lpos, {pos = legendstyle})
        end

    end
    -- graduations
    local O = A+(k1-1)*pas
    local graduations = {} -- graduations sous forme de chemin
    for k = k1, k2-1 do
        O = O + pas
        if k%(1+nbsubdiv) == 0 
            then insert(graduations, {O-n1,"m", O+n2,"l"})
            else insert(graduations, {O-n1/2,"m", O+n2/2,"l"} )
        end
    end
    if (arrows == "-") or (self:Abs(O+pas-L[2]) > 0.2) 
    then 
        O = O + pas
        if k2%(1+nbsubdiv) == 0 then  insert(graduations, {O-n1,"m", O+n2,"l"})
        else insert(graduations, {O-n1/2,"m", O+n2/2,"l"} )
        end
    end
    self:Dpath(graduations,"-")
    -- labels
    local langle
    if labelstyle == "auto" then 
        langle = self:Arg(u)*180/math.pi 
        if (langle >= 90) then langle = langle-180 end
        if (langle <= -90) then langle = langle+180 end
    else langle = labelangle
    end
    local dep, lpos
    lpos = labelstyle
    if labelpos == "bottom" then 
           dep = A-n1-n*xylabelsep/self:Abs(n)
           if labelstyle == "auto" then lpos = self:Poslab(-n,langle)
           end
    else
        if labelpos == "top" then --dessus
            dep = A+n2+n*xylabelsep/self:Abs(n)
            if labelstyle == "auto" then lpos = self:Poslab(n,langle)
            end
        end
    end
    local uDir = u/self:Abs(u)
    local O = dep+labelshift*uDir
    --affichage Labels
    local optlab, sep, labelList = {}, "", {}
    optlab.pos = lpos
    optlab.node_options = ""
    --if langle ~= 0 then optlab.node_options = "rotate="..langle; sep = "," end
    --if labelcolor ~= "" then optlab.node_options = optlab.node_options..sep.."color="..labelcolor end
    self.param.labelangle = langle
    self.param.labelcolor = labelcolor
    if labelpos ~= "none" and (mylabels == "") then
     --label origine
         local dec
         if originpos == "center" then dec = 0
         else
            if originpos == "right" then 
                if labelshift == 0 then dec = math.abs(defaultlabelshift)*uDir else dec = 0 end
            else
                if originpos == "left" then 
                    if labelshift == 0 then dec = -math.abs(defaultlabelshift)*uDir else dec = 0 end
                else dec = ""
                end
            end
         end
         local texte, fracN, fracD
         if dec ~= "" then
            fracN, fracD = simplifyFrac(originnum,labelden)
            texte = gradLabel(fracN,fracD,labeltext)
            insert(labelList, {texte,O+dec,optlab})
            --self:Dlabel(texte,O+dec,optlab)
        end
        -- labels sur graduations à droite
        local fracN, fracD = originnum, labelden
        O = O+u;  fracN, fracD = addFrac(fracN,fracD,unit,labelden)
        local k2_, k1_ = k2//(1+nbsubdiv), k1//(1+nbsubdiv)
        for k = 1, k2_-1 do
            if k >= k1_ then
                texte = gradLabel(fracN, fracD,labeltext)
                insert(labelList, {texte,O,optlab})
                --self:Dlabel(texte,O,optlab)
            end
            O = O+u; fracN, fracD = addFrac(fracN, fracD,unit,labelden)
        end
        if (k2_ >= 1) and (arrows == "-") or (self:Abs(A+k2_*u-L[2]) > 0.2) 
        then 
            texte = gradLabel(fracN, fracD,labeltext)
            insert(labelList, {texte,O,optlab})
            --self:Dlabel(texte,O,optlab)
        end
        -- labels sur graduations à gauche
        O = dep-u+labelshift*uDir; fracN, fracD = originnum, labelden; fracN, fracD = addFrac(fracN, fracD,-unit,labelden)
        k1_ = (-k1)//(1+nbsubdiv)
        for k = 1, k1_ do
            texte = gradLabel(fracN, fracD,labeltext)
            insert(labelList, {texte,O,optlab})
            --self:Dlabel(texte,O,optlab)
            O = O-u; fracN, fracD = addFrac(fracN, fracD,-unit,labelden)
        end
    end
    -- labels personnels
    if (labelpos ~= "none") and (mylabels ~= "") then
        local dots = {}
        for k = 1, #mylabels//2 do
            local z, texte = toComplex(mylabels[2*k-1]), mylabels[2*k]
            local x_, sdot = z.re, (z.im ~= 0) -- si la partie imaginaire n'est pas nulle, on affiche un point
            O = dep+labelshift*uDir+x_*u
            insert(labelList, {texte,O,optlab})
            --self:Dlabel(texte,O,optlab)
            if sdot then table.insert(dots, A+x_*u) end
        end
        if #dots > 0 then self:Ddots(dots) end
    end
    if #labelList > 0 then self:Dlabel( table.unpack(labelList) ) end
    self:Linestyle(oldstyle); self:Labelangle(oldlabelangle); self:Labelcolor(oldlabelcolor)
    self:Restorematrix()
end    


function luadraw_graph2d:DaxeX(d, options)
-- dessin d'un axe Ox avec d={A,pas}
    local A, xpas
    if (d == nil) then A = Z(0,0); xpas = 1
    else
        if (type(d) ~= "table") or (#d ~= 2) then return 
        else
            A = d[1] ; xpas = d[2] ; A = toComplex(A)
            if (A == nil) or (xpas == nil) then return end
            if xpas == 0 then xpas = 1 end
        end
    end
    options = options or {}
    options.showaxe = options.showaxe or 1 --affichage ou non de l'axe (1 ou 0)
    options.arrows = options.arrows or "-"
    options.limits = options.limits or "auto" -- {x1,x2} intervalle des abscisses à couvrir "auto" par défaut pour toute la droite entière
    options.unit = options.unit or "" -- graduation de 1 en 1 par défaut
    options.gradlimits = options.gradlimits or "auto" -- {N1,N2} intervalle des graduations (entières), égal à all par défaut
    options.nbsubdiv = options.nbsubdiv or 0 -- nombre de subdivisions par unité

    options.tickpos = options.tickpos or 0.5 -- nombre entre 0 et 1
    options.tickdir = options.tickdir or "auto" -- direction graduations (ortho par défaut)
    options.xyticks = options.xyticks or 0.2 -- longueur des graduations
    options.xylabelsep = options.xylabelsep or defaultxylabelsep -- distance labels-graduations

    options.originpos = options.originpos or "center" -- "none" or "center" or "left" or "right"
    options.originnum = options.originnum or A.re -- les labels sont: (originnum + unit*n)"labeltext"/labelden
    
    options.legend = options.legend or "" -- légende
    options.legendpos = options.legendpos or 0.975 -- nombre entre 0 et 1
    options.legendsep = options.legendsep or defaultlegendsep 
    options.legendangle = options.legendangle or "auto" 

    options.labelpos = options.labelpos or "bottom" -- "none" or "top" or "bottom"
    options.labelden = options.labelden or 1 -- dénominateur (entier)
    options.labeltext = options.labeltext or "" -- texte ajouté aux labels, vide par défaut
    options.labelstyle = options.labelstyle or "S" -- "auto"  or "E" or "W",...
    options.labelangle = options.labelangle or 0 -- angle des labels en degrés par rapport à l'horizontale
    options.labelcolor = options.labelcolor or ""
    options.labelshift = options.labelshift or 0 -- décalage systématique des labels
    options.nbdeci = options.nbdeci or 2 -- nb de décimales, 2 par défaut
    options.numericFormat = options.numericFormat or 0 -- format d'affichage
    options.mylabels = options.mylabels or "" -- labels personnels, liste {pos1,texte1, pos2,texte2,...} or chaine vide
    
    if options.labelpos == "top" then options.labelstyle = "N" end
    if options.unit == "" then 
        if options.labeltext == "" then options.unit = xpas else options.unit = 1 end
    end
    if options.limits ~= "auto" then 
        local x1, x2 = options.limits[1], options.limits[2]
        options.limits = { math.ceil( (x1-A.re)/xpas ), math.floor( (x2-A.re)/xpas) }
    end
    if options.gradlimits ~= "auto" then 
        local x1, x2 = options.gradlimits[1], options.gradlimits[2]
        options.gradlimits = { math.ceil( (x1-A.re)/xpas ), math.floor( (x2-A.re)/xpas) }
    end
    if (xpas < 0) and (options.labelpos ~= "none") then 
            if options.labelpos == "top" then options.labelpos = "bottom" else options.labelpos = "top" end
            if (options.originpos ~= "none") and (options.originpos ~= "center") then 
                if options.originpos == "right" then options.originpos = "left" else options.originpos = "right" end
            end
    end
    self:Dgradline({A,xpas}, options)
end    

function luadraw_graph2d:DaxeY(d, options)
-- dessin d'un axe Ox avec d={A,pas}
    local A, ypas
    if (d == nil) then A = Z(0,0); ypas = 1
    else
        if (type(d) ~= "table") or (#d ~= 2) then return 
        else
            A = d[1] ; ypas = d[2] ; A = toComplex(A)
            if (A == nil) or (ypas == nil) then return end
            if ypas == 0 then ypas = 1 end
        end
    end
    options = options or {}
    options.showaxe = options.showaxe or 1 --affichage ou non de l'axe (1 ou 0)
    options.arrows = options.arrows or "-"    
    options.limits = options.limits or "auto" -- {x1,x2} intervalle des abscisses à couvrir "auto" par défaut pour toute la droite entière
    options.unit = options.unit or "" -- graduation de 1 en 1 par défaut
    options.gradlimits = options.gradlimits or "auto" -- {N1,N2} intervalle des graduations (entières), égal à all par défaut
    options.nbsubdiv = options.nbsubdiv or 0 -- nombre de subdivisions par unité

    options.tickpos = options.tickpos or 0.5 -- nombre entre 0 et 1
    options.tickdir = options.tickdir or "auto" -- direction graduations (ortho par défaut)
    options.xyticks = options.xyticks or 0.2 -- longueur des graduations
    options.xylabelsep = options.xylabelsep or defaultxylabelsep -- distance labels-graduations

    options.originpos = options.originpos or "center" -- "none" or "center" or "left" or "right"
    options.originnum = options.originnum or A.im -- les labels sont: (originnum + unit*n)"labeltext"/labelden
    
    options.legend = options.legend or "" -- légende
    options.legendpos = options.legendpos or 0.975 -- nombre entre 0 et 1
    options.legendsep = options.legendsep or defaultlegendsep 
    options.legendangle = options.legendangle or "auto" 

    options.labelpos = options.labelpos or "left" -- "none" or "right" or "left"
    options.labelden = options.labelden or 1 -- dénominateur (entier)
    options.labeltext = options.labeltext or "" -- texte ajouté aux labels, vide par défaut
    options.labelstyle = options.labelstyle or "W" -- "auto"  or "E" or "W",...
    options.labelangle = options.labelangle or 0 -- angle des labels en degrés par rapport à l'horizontale    
    options.labelcolor = options.labelcolor or ""
    options.labelshift = options.labelshift or 0 -- décalage systématique des labels
    options.nbdeci = options.nbdeci or 2 -- nb de décimales, 2 par défaut
    options.numericFormat = options.numericFormat or 0 -- format d'affichage
    options.mylabels = options.mylabels or "" -- labels personnels, liste {pos1,texte1, pos2,texte2,...} or chaine vide

    if options.labelpos == "right" then options.labelstyle = "E" end
    if options.unit == "" then 
        if options.labeltext == "" then options.unit = ypas else options.unit = 1 end
    end
    if options.limits ~= "auto" then 
        local x1, x2 = options.limits[1], options.limits[2]
        options.limits = { math.ceil( (x1-A.im)/ypas ), math.floor( (x2-A.im)/ypas) }
    end
    if options.gradlimits ~= "auto" then 
        local x1, x2 = options.gradlimits[1], options.gradlimits[2]
        options.gradlimits = { math.ceil( (x1-A.im)/ypas ), math.floor( (x2-A.im)/ypas) }
    end
    if (options.originpos ~= "none") and (options.originpos ~= "center") then 
        if options.originpos == "top" then options.originpos = "right" else options.originpos = "left" end
    end
    if options.labelpos ~= "none" then
        if options.labelpos == "left" then options.labelpos = "top"
        else options.labelpos = "bottom"
        end
        if (ypas < 0)  then 
            if options.labelpos == "top" then options.labelpos = "bottom" else options.labelpos = "top" end
            if (options.originpos ~= "none") and (options.originpos ~= "center") then 
                if options.originpos == "right" then options.originpos = "left" else options.originpos = "right" end
            end
        end
   end
    self:Dgradline({A, Z(0,ypas)}, options)
end    

function luadraw_graph2d:Daxes(d, options)
-- dessin des axes Ox et Oy avec d={A,xpas,ypas}
    local A, xpas, ypas
    if (d == nil) then A = Z(0,0); xpas, ypas = 1,1
    else
        if (type(d) ~= "table") or (#d ~= 3) then return 
        else
            A = d[1] ; xpas = d[2] ; ypas = d[3]; A = toComplex(A)
            if (A == nil) or (xpas == nil) or (ypas == nil) then return end
            if xpas == 0 then xpas = 1 end
            if ypas == 0 then ypas = 1 end
        end
    end
    options = options or {} -- les options sont des pairs de deux valeurs, une pour Ox et une pour Oy
    options.showaxe = options.showaxe or {1,1} --affichage ou non de l'axe (1 ou 0)
    options.arrows = options.arrows or "-"
    options.limits = options.limits or {"auto","auto"} -- {x1,x2} intervalle des abscisses à couvrir "auto" par défaut pour toute la droite entière
    options.unit = options.unit or {"",""} -- graduation de 1 en 1 par défaut
    options.gradlimits = options.gradlimits or {"auto","auto"} -- {N1,N2} intervalle des graduations (entières), égal à all par défaut
    options.nbsubdiv = options.nbsubdiv or {0,0} -- nombre de subdivisions par unité

    options.tickpos = options.tickpos or {0.5,0.5} -- nombre entre 0 et 1
    options.tickdir = options.tickdir or {"auto","auto"} -- direction graduations (ortho par défaut)
    options.xyticks = options.xyticks or {0.2,0.2} -- longueur des graduations
    options.xylabelsep = options.xylabelsep or {defaultxylabelsep,defaultxylabelsep} -- distance labels-graduations

    options.originpos = options.originpos or {"right","top"} -- "none" or "center" or "left" or "right", "nonne, "bottom, or "top"
    options.originnum = options.originnum or {A.re,A.im} -- les labels sont: (originnum + unit*n)"labeltext"/labelden
    options.originloc = options.originloc or A -- point de croisement des axes
    
    options.legend = options.legend or {"",""} -- légende
    options.legendpos = options.legendpos or {0.975,0.975} -- nombre entre 0 et 1
    options.legendsep = options.legendsep or {defaultlegendsep,defaultlegendsep} 
    options.legendangle = options.legendangle or {"auto" ,"auto"}

    options.labelpos = options.labelpos or {"bottom","left"} -- "none" or "right" or "left"
    options.labelden = options.labelden or {1,1} -- dénominateur (entier)
    options.labeltext = options.labeltext or {"",""} -- texte ajouté aux labels, vide par défaut
    options.labelstyle = options.labelstyle or {"S","W"} -- "auto"  or "E" or "W",...
    options.labelangle = options.labelangle or {0,0} -- angle des labels en degrés par rapport à l'horizontale    
    options.labelcolor = options.labelcolor or {"",""}    
    options.labelshift = options.labelshift or {0,0} -- décalage systématique des labels
    options.nbdeci = options.nbdeci or {2,2} -- nb de décimales, 2 par défaut
    options.numericFormat = options.numericFormat or {0,0} -- format d'affichage
    options.myxlabels = options.myxlabels or "" -- labels personnels, liste {pos1,texte1, pos2,texte2,...} or chaine vide
    options.myylabels = options.myylabels or "" -- labels personnels, liste {pos1,texte1, pos2,texte2,...} or chaine vide
    
    options.grid = options.grid or false
    options.drawbox =options.drawbox or false
    options.gridstyle = options.gridstyle or "solid"
    options.subgridstyle = options.subgridstyle or "solid"
    options.gridcolor = options.gridcolor or "gray"
    options.subgridcolor = options.subgridcolor or "lightgray" 
    options.gridwidth = options.gridwidth or 4
    options.subgridwidth = options.subgridwidth or 2
    local x1,x2,y1,y2 = table.unpack(self.param.coordsystem)
    local z1,z2 = Z(x1,y1), Z(x2,y2)
    if options.drawbox then
        -- dessin de la boite graduée
        local l1, l2 = options.limits[1], options.limits[2]
        if l1 ~= "auto" --then x1 = z1.re; x2 = z2.re
        then x1 = l1[1]; x2 = l1[2]
        end
        if l2 ~= "auto" --then y1 = z1.im; y2 = z2.im
        then y1 = l2[1];  y2 = l2[2]
        end
        local oldlimits = options.limits
        options.limits = { {x1,x2}, {y1,y2} }
        self:Dgradbox( { Z(x1,y1), Z(x2,y2), xpas, ypas}, options)
        options.limits = oldlimits
    else
        if options.grid then
            -- dessin de la grille
            -- pour que les labels ne soient pas sur les traits de la grille
            if options.labelshift[1] == 0 then options.labelshift = {defaultlabelshift,options.labelshift[2]} end
            if options.labelshift[2] == 0 then options.labelshift = {options.labelshift[1],defaultlabelshift} end
            if options.gradlimits[1] == "auto" then 
                if options.limits[1] ~= "auto" --then x1 = z1.re; x2 = z2.re
                then x1 = options.limits[1][1]; x2 = options.limits[1][2]
                end
            else 
                x1 = options.gradlimits[1][1]; x2 = options.gradlimits[1][2]
            end
            if options.gradlimits[2] == "auto" then 
                if options.limits[2] ~= "auto" --then y1 = z1.im; y2 = z2.im
                then y1 = options.limits[2][1]; y2 = options.limits[2][2]
                end
            else 
                y1 = options.gradlimits[2][1]; y2 = options.gradlimits[2][2]
            end
            local oldunit = options.unit
            options.unit = {xpas, ypas}
            self:Dgrid( {Z(x1,y1), Z(x2,y2)}, options)
            options.unit = oldunit
        end
        -- dessin des axes
        self:DaxeX( {options.originloc,xpas}, {
             limits = options.limits[1],
             gradlimits = options.gradlimits[1],
             unit = options.unit[1],
             showaxe = options.showaxe[1],
             arrows = options.arrows,
             tickpos = options.tickpos[1],
             nbsubdiv = options.nbsubdiv[1],
             originpos = options.originpos[1],
             labelpos = options.labelpos[1],
             legendpos = options.legendpos[1],
             legend = options.legend[1],
             nbdeci = options.nbdeci[1],
             originnum = options.originnum[1],
             labelden = options.labelden[1],
             labeltext = options.labeltext[1],
             labelstyle = options.labelstyle[1],
             labelangle = options.labelangle[1],
             labelcolor = options.labelcolor[1],
             xyticks = options.xyticks[1],
             xylabelsep = options.xylabelsep[1], 
             legendsep = options.legendsep[1],
             tickdir = options.tickdir[1],
             numericFormat = options.numericFormat[1],
             legendangle = options.legendangle[1],
             labelshift = options.labelshift[1],
             mylabels = options.myxlabels})
             
        self:DaxeY( {options.originloc,ypas}, {
             limits = options.limits[2],
             gradlimits = options.gradlimits[2],
             unit = options.unit[2],
             showaxe = options.showaxe[2],
             arrows = options.arrows,             
             tickpos = options.tickpos[2],
             nbsubdiv = options.nbsubdiv[2],
             originpos = options.originpos[2],
             labelpos = options.labelpos[2],
             legendpos = options.legendpos[2],
             legend = options.legend[2],
             nbdeci = options.nbdeci[2],
             originnum = options.originnum[2],
             labelden = options.labelden[2],
             labeltext = options.labeltext[2],
             labelstyle = options.labelstyle[2],
             labelangle = options.labelangle[2],
             labelcolor = options.labelcolor[1],
             xyticks = options.xyticks[2],
             xylabelsep = options.xylabelsep[2], 
             legendsep = options.legendsep[2],
             tickdir = options.tickdir[2],
             numericFormat = options.numericFormat[2],
             legendangle = options.legendangle[2],
             labelshift = options.labelshift[2],
             mylabels = options.myylabels})             
    end
end

function luadraw_graph2d:Dgrid(d,options) -- Dgrid( {coin inf gauche, coin sup droit}, <options> )
-- dessin d'une grille avec d={coin inf gauche, coin sup droit}
    local A, B
    if (d == nil) or (type(d) ~= "table") or (#d ~= 2) then return  end
    A = d[1] ; A = toComplex(A)
    B = d[2] ; B = toComplex(B)
    if (A == nil) or (B == nil) then return end
    options = options or {}
    local unit = options.unit or {1,1} -- unités sur les axes
    local gridwidth = options.gridwidth or 4 -- épaisseur
    local gridcolor = options.gridcolor or "gray" -- couleur grille principale
    local gridstyle = options.gridstyle or "solid"
    local nbsubdiv = options.nbsubdiv or {0,0} -- nombre de subdivisions par unité
    local subgridcolor = options.subgridcolor or "lightgray" --couleur grille secondaire
    local subgridwidth = options.subgridwidth or 2 -- epaisseur 
    local subgridstyle = options.subgridstyle or "solid"
    local originloc = options.originloc or A  -- localisation de l'origine

    local xnbsubdiv = nbsubdiv[1]+1
    local ynbsubdiv = nbsubdiv[2]+1
    local xdep, ydep = A.re, A.im
    local xfin, yfin = B.re, B.im
    if xdep > xfin then xdep, xfin = xfin, xdep end
    if ydep > yfin then ydep, yfin = yfin, ydep end
    local xpas, ypas = table.unpack(unit)
    xpas = math.abs(xpas); ypas = math.abs(ypas)
    originloc = toComplex(originloc)
    local x1, y1 = originloc.re, originloc.im
    local k = math.floor( (x1-xdep)/xpas )
    local xmin = x1-k*xpas
    k = math.floor( (xfin-x1)/xpas )
    local xmax = x1+k*xpas
    k = math.floor( (y1-ydep)/ypas )
    local ymin = y1-k*ypas
    k = math.floor( (yfin-y1)/ypas )
    local ymax = y1+k*ypas
    local xdiv = (xmax-xmin)//xpas
    local ydiv = (ymax-ymin)//ypas
    self:Saveattr() --; self:Arrows("-"); self:Filloptions("none")
    --grille secondaire
    local subgridpasx = xpas/xnbsubdiv
    local subgridpasy = ypas/ynbsubdiv
    local grille = {}
    local x = xmin
    for k = 1, math.floor((xmax-xmin)*xnbsubdiv/xpas) do
        if (k-1)%xnbsubdiv ~= 0 then insert(grille, {Z(x,ydep),"m",Z(x,yfin),"l"}) end
        x = x + subgridpasx
    end
    local y = ymin
    for k = 1, math.floor((ymax-ymin)*ynbsubdiv/ypas) do
        if (k-1)%ynbsubdiv ~= 0 then  insert(grille, {Z(xdep,y),"m",Z(xfin,y),"l"}) end
        y = y + subgridpasy
    end
    if #grille > 0 then
        self:Lineoptions(subgridstyle,subgridcolor,subgridwidth)
        self:Dpath(grille,"-")
    end
    -- grille principale}
    grille = {}
    x = xmin
    for k = 0, xdiv do insert(grille,{Z(x,ydep),"m",Z(x,yfin),"l"}); x = x+xpas end
    y = ymin
    for k = 0, ydiv do insert(grille, {Z(xdep,y),"m",Z(xfin,y),"l"}); y = y+ypas end
    if #grille > 0 then
        self:Lineoptions(gridstyle,gridcolor,gridwidth); self:Linecap("round")
        self:Dpath(grille,"-")
    end
    self:Restoreattr()
end

function luadraw_graph2d:Dgradbox(d, options)
-- dessin d'une boite graduée avec d={coin inf gauche, coin sup droit, xpas, ypas}
    local A, B, xpas, ypas
    if (d == nil) or (type(d) ~= "table") or (#d < 2) then return 
    else
        A = d[1] ; B = d[2]
        xpas = d[3] or 1
        ypas = d[4] or 1
        A = toComplex(A) ; B = toComplex(B)
        if (A == nil) or (B == nil) then return end
        if xpas == 0 then xpas = 1 end
        if ypas == 0 then ypas = 1 end
    end
    options = options or {} -- les options sont des pairs de deux valeurs, une pour Ox et une pour Oy
    options.showaxe = options.showaxe or {1,1} --affichage ou non de l'axe (1 ou 0)

    options.unit = options.unit or {"",""} -- graduation de 1 en 1 par défaut
    options.limits = {{A.re,B.re}, {A.im,B.im}} -- {x1,x2} intervalle des abscisses à couvrir "auto" par défaut pour toute la droite entière    
    options.gradlimits = options.gradlimits or {"auto","auto"} -- {N1,N2} intervalle des graduations (entières), égal à all par défaut
    options.nbsubdiv = options.nbsubdiv or {1,1} -- nombre de subdivisions par unité

    options.tickpos = options.tickpos or {0,1} -- nombre entre 0 et 1
    options.tickdir = options.tickdir or {"auto","auto"} -- direction graduations (ortho par défaut)
    options.xyticks = options.xyticks or {0.2,0.2} -- longueur des graduations
    options.xylabelsep = options.xylabelsep or {defaultxylabelsep,defaultxylabelsep} -- distance labels-graduations

    options.originpos = options.originpos or {"center","center"} -- "none" or "center" or "left" or "right", "nonne, "bottom, or "top"
    options.originnum = options.originnum or {A.re,A.im} -- les labels sont: (originnum + unit*n)"labeltext"/labelden
    options.originloc = options.originloc or false -- point de croisement des axes
    
    options.legend = options.legend or {"",""} -- légende
    options.legendpos = options.legendpos or {0.5,0.5} -- nombre entre 0 et 1
    options.legendsep = options.legendsep or {-0.8,-1} 
    options.legendangle = options.legendangle or {self:Arg(1)*180/math.pi ,self:Arg(cpx.I)*180/math.pi}
    options.title = options.title or "" -- titre en haut de la boite
    options.legendstyle = options.legendstyle or {"S","center"} -- légendes dessous et à gauche

    options.labelpos = options.labelpos or {"bottom","left"} -- "none" or "right" or "left"
    options.labelden = options.labelden or {1,1} -- dénominateur (entier)
    options.labeltext = options.labeltext or {"",""} -- texte ajouté aux labels, vide par défaut
    options.labelstyle = options.labelstyle or {"S","W"} -- "auto"  or "E" or "W",...
    options.labelangle = options.labelangle or {0,0} -- angle des labels en degrés par rapport à l'horizontale    
    options.labelcolor = options.labelcolor or {"",""}
    options.labelshift = options.labelshift or {0,0} -- décalage systématique des labels
    options.nbdeci = options.nbdeci or {2,2} -- nb de décimales, 2 par défaut
    options.numericFormat = options.numericFormat or {0,0} -- format d'affichage
    options.myxlabels = options.myxlabels or "" -- labels personnels, liste {pos1,texte1, pos2,texte2,...} or chaine vide
    options.myylabels = options.myylabels or "" -- labels personnels, liste {pos1,texte1, pos2,texte2,...} or chaine vide
    
    options.grid = options.grid or false
    options.gridstyle = options.gridstyle or "solid"
    options.subgridstyle = options.subgridstyle or "solid"
    options.gridcolor = options.gridcolor or "gray"
    options.subgridcolor = options.subgridcolor or "lightgray" 
    options.gridwidth = options.gridwidth or 4
    options.subgridwidth = options.subgridwidth or 2
    
    local x1, y1, x2, y2 = A.re, A.im, B.re, B.im
    if options.grid then
        self:Dgrid( {A, B}, {unit = {xpas,ypas}, nbsubdiv = options.nbsubdiv, gridstyle = options.gridstyle, subgridstyle = options.subgridstyle,
            gridcolor = options.gridcolor, subgridcolor = options.subgridcolor, gridwidth = options.gridwidth, subgridwidth = options.subgridwidth, 
            originloc = options.originloc} )
    end
    self:Dpolyline( {A, Z(x2,y1), B, Z(x1,y2)}, true,"-") -- cadre
    local x1_, y1_
    if options.originloc == false then 
        x1_ = x1 ; y1_ = y1 
    else options.originloc = toComplex(options.originloc); x1_ = options.originloc.re ; y1_ = options.originloc.im
    end
    self:DaxeX( { Z(x1_,y2), xpas}, -- axe du haut
        {legend = options.title, legendsep = -0.05, legendpos = 0.5, legendstyle = "N", 
        limits = options.limits[1], gradlimits = options.gradlimits[1], unit = options.unit[1], showaxe = 0, 
        tickpos = 1-options.tickpos[1], nbsubdiv = options.nbsubdiv[1], labeltext = options.labeltext[1],
        labelpos = "none", xyticks = options.xyticks[1], tickdir = options.tickdir[1] } )
        
    self:DaxeX( { Z(x1_,y1), xpas}, -- axe du bas
        {limits = options.limits[1], gradlimits = options.gradlimits[1], unit = options.unit[1], showaxe = 0,
        tickpos = options.tickpos[1],nbsubdiv = options.nbsubdiv[1], originpos = options.originpos[1], 
        labelpos = options.labelpos[1], legendpos = options.legendpos[1], legend = options.legend[1], 
        legendstyle = options.legendstyle[1], nbdeci = options.nbdeci[1], 
        originnum = options.originnum[1], labelden = options.labelden[1], labeltext = options.labeltext[1], 
        labelstyle = options.labelstyle[1], xyticks = options.xyticks[1], labelcolor = options.labelcolor[1],
        xylabelsep = options.xylabelsep[1], legendsep = options.legendsep[1], labelangle = options.labelangle[1], labelstyle = options.labelstyle[1], 
        tickdir = options.tickdir[1], numericFormat = options.numericFormat[1], mylabels = options.myxlabels, legendangle = options.legendangle[1] })
       
    self:DaxeY( {Z(x1,y1_), ypas}, -- axe de gauche
        {limits = options.limits[2], gradlimits = options.gradlimits[2], unit = options.unit[2], showaxe = 0, tickpos = options.tickpos[2], 
        nbsubdiv = options.nbsubdiv[2], originpos = options.originpos[2], labelpos = options.labelpos[2], labelstyle = options.labelstyle[2], 
        legendpos = options.legendpos[2], legend = options.legend[2], nbdeci = options.nbdeci[2], originnum = options.originnum[2], 
        labelden = options.labelden[2], labeltext = options.labeltext[2], labelstyle = options.labelstyle[2], xyticks = options.xyticks[2],
        xylabelsep = options.xylabelsep[2], labelcolor = options.labelcolor[2], legendsep = options.legendsep[2],
        legendstyle = options.legendstyle[2], labelangle = options.labelangle[2],  
        tickdir = options.tickdir[2], numericFormat = options.numericFormat[2],mylabels = options.myylabels, legendangle = options.legendangle[2] })
        
    self:DaxeY( {Z(x2,y1_), ypas},  -- axe de droite
        {legend = "", limits = options.limits[2], gradlimits = options.gradlimits[2], unit = options.unit[2], showaxe = 0, 
        tickpos = 1-options.tickpos[2], nbsubdiv = options.nbsubdiv[2], labeltext = options.labeltext[2],
        labelpos = "none", xyticks = options.xyticks[2], tickdir = options.tickdir[2] } )

end    

return luadraw_graph2d
