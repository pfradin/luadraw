-- luadraw_log_axes.lua 
-- date 2026/04/09
-- version 2.8
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   https://www.ctan.org/license/lppl


-- draw logarithmic axes
local logtype = "logy" -- or "logx" or "logxy"
local xmin, xmax, ymin, ymax
local powerminx, powerminy, decadex, decadey
defaultloglabels = {2,3,5,10}
local nbdivy, nbdivx
local grid ,gridwidth, gridcolor, gridstyle, subgridwidth, subgridcolor, subgridstyle = true, 4, "gray", "solid", 2, "lightgray", "solid"
local clip, clipbox
local log = math.log10

function graph:Beginlogview(type,x1,x2,y1,y2,options)
    local error = false
    options = options or {}
    clip = options.clip
    local viewport = options.viewport
    --local u1,u2,v1,v2 = table.unpack(self.param.viewport)
    if clip == nil then clip = true end
    local dx, dy
    logtype = type
    xmin = x1; xmax = x2; ymin = y1; ymax = y2
    if logtype == "logx" then
        x1 = log(x1); x2 = log(x2)
        decadex = x2-x1; powerminx = x1
        dx, dy = decadex/10, (y2-y1)/10
        self:Saveattr()
        if viewport ~= nil then self:Viewport(table.unpack(viewport)) end
        self:Coordsystem(-dx,decadex,y1-dy,y2)
        if clip then clipbox = {0,decadex,y1,y2} end
     elseif logtype == "logy" then
        y1 = log(y1); y2 = log(y2)
        decadey = y2-y1; powerminy = y1
        dx, dy = (x2-x1)/10, decadey/10
        self:Saveattr()
        if viewport ~= nil then self:Viewport(table.unpack(viewport)) end
        self:Coordsystem(x1-dx,x2,-dy,decadey)
        if clip then clipbox = {x1,x2,0,decadey} end
     elseif logtype == "logxy" then
        x1 = log(x1); x2 = log(x2)
        decadex = x2-x1; powerminx = x1
        y1 = log(y1); y2 = log(y2)
        decadey = y2-y1; powerminy = y1
        dx, dy = decadex/10, decadey/10
        self:Saveattr()
        if viewport ~= nil then self:Viewport(table.unpack(viewport)) end
        self:Coordsystem(-dx,decadex,-dy,decadey)
        if clip then clipbox = {0,decadex,0,decadey} end
    else 
        print('Beginlogview Warning: type must be "logx" or "logy" or logxy"')
        error = true
    end
    if not error then
        self:Dlogaxes(options)
    end
end

function graph:Endlogview()
    self:Restoreattr()
end

--------------------------- grid and axes ------------------------------

local dgrid = function(g,grid1,grid2)
    local oldlinestyle = g.param.linestyle
    local oldlinecolor = g.param.linecolor
    local oldlinewidth = g.param.linewidth
    local change = false 
    if #grid1 > 0 then 
        g:Lineoptions(gridstyle, gridcolor, gridwidth); change = true
        g:Dpolyline(grid1, false,"-")
    end
    if #grid2 > 0 then
        g:Lineoptions(subgridstyle, subgridcolor, subgridwidth); change = true
        g:Dpolyline(grid2, false,"-")
    end
    if change then g:Lineoptions(oldlinestyle, oldlinecolor, oldlinewidth) end
end


function graph:Dlogaxe(type,options)
    local decade, decadelabel, powermin, nbdiv, nbsubdiv, pas, n, umin, umax, vmin, vmax, O, dir, style, pos, Ueps, angle
    if type == "logx" then
        decade = decadex; umin = xmin; umax = xmax; powermin = powerminx
        if logtype == "logxy" then
            vmin = 0; vmax = decadey;
        else
            vmin = ymin; vmax = ymax;
        end
        n = Z(0,1); O = Z(0,vmin); dir = 1; pos = options.labelpos or "bottom"
    elseif type == "logy" then
        decade = decadey; umin = ymin; umax = ymax;  powermin = powerminy
        if logtype == "logxy" then
            vmin = 0; vmax = decadex;
        else
            vmin = xmin; vmax = xmax
        end
        n = Z(-1,0); O = Z(vmin,0); dir = cpx.I; pos = options.labelpos or "top"
        if pos == "left" then pos = "top" end
        if pos == "right" then pos = "bottom" end
    end
    Ueps = (umax-umin)*1e-7
    if decade < 1-1e-6 then  
        pas = (umax-umin)/10; nbdiv = nil
     else 
        nbdiv = 9; pas = nil
    end
    options = options or {}
    pas = options.step or pas
    style = options.labelstyle
    angle = options.labelangle
    nbsubdiv = options.nbsubdiv or 3
    nbsubdiv = nbsubdiv+1
    decadeloglabel = options.decadeloglabels
    local addlabels = options.addloglabels
    local tickpos = options.tickpos or 0.5 -- nombre entre 0 et 1
    local xyticks = options.xyticks or 0.2 -- longueur des graduations 
    local xylabelsep = options.xylabelsep or defaultxylabelsep -- longueur des graduations 
    local exponent = options.exponent or 0
    if exponent ~= 0 then options.legend = options.legend.." ($\\times10^{"..exponent.."}$) " end
    local old_siunitx = siunitx
    siunitx = options.use_siunitx
    local n1 = n/self:Abs(n)*xyticks*tickpos
    local n2 = n/self:Abs(n)*xyticks*(1-tickpos)
    local pasU
    if pas ~= nil then 
        pasU = pas/umin; nbdiv = 9*umin/pas
        if (decade < 1-1e-6) and  (decadeloglabel == nil) and ((umax-umin)/pas < maxGrad) then 
            local u = umin+pas
            decadeloglabel = {u}
            while u < umax+Ueps do 
                table.insert(decadeloglabel,u)
                u = u+pas
            end
        end
    else pasU = 9/nbdiv
    end
    if decadeloglabel == nil then 
        decadeloglabel = {}
        for _, v in ipairs(defaultloglabels) do
            table.insert(decadeloglabel, umin*v)
        end
    end
    local ulabels = {0, gradLabel(umin*10^(-exponent),1,"")}
    local ticks = {{O-n1, O+n2}}
    local maxval = umax*10^(-exponent)
    Ueps = Ueps*10^(-exponent)
    for k = 0, decade do
        for _,v in ipairs(decadeloglabel) do
            local y = k+log(v)-powermin
            local y0 = v*10^(k-exponent)
            if y0 < maxval+Ueps then
                table.insert(ulabels, y)
                table.insert(ulabels, gradLabel(y0,1,""))
                table.insert(ticks, {O+y*dir-n1, O+y*dir+n2})
            end
        end
    end
    if addlabels ~= nil then
        for _, v in ipairs(addlabels) do
            local y = log(v)-powermin
            local y0 = v*10^(-exponent)
            if y0 < maxval+Ueps then
                table.insert(ulabels, y)
                table.insert(ulabels, gradLabel(y0,1,""))
                table.insert(ticks, {O+y*dir-n1, O+y*dir+n2})
            end
        end
    end
    siunitx = old_siunitx
    self:Dgradline({O,dir}, {limits={0,decade}, originpos="center", 
            labelpos=pos, mylabels=ulabels, labelstyle=style, labelangle=angle, xyticks=0, xylabelsep=options.xylabelsep,
            legend=options.legend, legendpos=options.legendpos, legendstyle=options.legendstyle ,
            legendsep=options.legendsep, legendangle=options.legendangle, labelcolor=options.labelcolor, 
            node_options=options.node_options
        })
    self:Dpolyline(ticks,false,"-")
    if grid then
        local subgridpas = pasU/nbsubdiv
        local grid1, grid2 = {}, {}
        local y = 1
        for k = 1, nbsubdiv*nbdiv do
            for p = 0, decade do
                local y1 = log(y)+p
                if y1 <= decade+1e-6 then
                    if (k-1)%nbsubdiv ~= 0 then
                        if type == "logy" then table.insert(grid2, {Z(vmin,y1),Z(vmax,y1)})
                        else table.insert(grid2, {Z(y1,vmin),Z(y1,vmax)})
                        end
                    else
                        if type == "logy" then table.insert(grid1, {Z(vmin,y1),Z(vmax,y1)})
                        else table.insert(grid1, {Z(y1,vmin),Z(y1,vmax)})
                        end
                    end
                end
             end
            y = y + subgridpas 
        end
        dgrid(self,grid1,grid2)
    end
end

function graph:Dlogaxes(options)
    local eps = 1e-7
    options = options or {}
    options.grid = options.grid or {true, true}
    if type(options.grid) == "boolean" then 
        options.grid = {options.grid, options.grid}
    end
    local legend = options.legend or {"",""}
    local legendpos = options.legendpos or {0.5,0.5}
    local legendsep = options.legendsep or {-0.5,-1}
    local legendstyle = options.legendstyle or {"S","N"}
    local legendangle = options.legendangle or {0,90}
    
    options.labelden = options.labelden or {1,1} -- dénominateur (entier)
    options.labeltext = options.labeltext or {"",""} -- texte ajouté aux labels, vide par défaut
    options.labelstyle = options.labelstyle or {"S","W"} -- "auto"  or "E" or "W",...
    options.labelangle = options.labelangle or {0,0} -- angle des labels en degrés par rapport à l'horizontale    
    options.labelcolor = options.labelcolor or {"",""}
    options.use_siunitx = options.use_siunitx or {siunitx,siunitx} -- format d'affichage géré par siunitx ou pas 
    
    options.xynode_options = options.xynode_options or ""
    options.xnode_options = options.xnode_options or options.xynode_options
    options.ynode_options = options.ynode_options or options.xynode_options 
    
    gridwidth  = options.gridwidth or 4
    gridcolor = options.gridcolor or "gray"
    gridstyle = options.gridstyle or "solid"
    subgridwidth = options.subgridwidth or 2
    subgridcolor = options.subgridcolor or "lightgray"
    subgridstyle = options.subgridstyle or "solid"
    
    options.tickpos = options.tickpos or {0.5,0.5} -- nombre entre 0 et 1
    options.xyticks = options.xyticks or {0.2,0.2} -- longueur des graduations
    options.xylabelsep = options.xylabelsep or {defaultxylabelsep,defaultxylabelsep} 
    options.arrows = "-"
    
    if logtype == "logx" then
        eps = (ymax-ymin)*eps
        grid = options.grid[1]
        options.showlines = {false, true}
        options.nbsubdiv = options.nbsubdiv or {3,0}
        options.limits = {ymin-eps,ymax+eps}
        options.gradlimits = {ymin,ymax}
        options.originloc = toComplex( options.originloc or Z(0,ymin) )
        options.ystep = options.ystep or 1
        options.labelpos = options.labelpos or {"bottom","left"} -- "none" or "right" or "left"
        local unit = options.unit or {"",""} 
        if options.grid[2] then
            options.unit = {1,options.ystep}
            self:Dgrid({Z(0,options.originloc.im),Z(decadex+1e-6,ymax+eps)}, options)
        end        
        self:Dlogaxe("logx", {step=options.xstep, nbsubdiv=options.nbsubdiv[1], decadeloglabels=options.xdecadeloglabels,
            addloglabels=options.xaddloglabels, legend=legend[1], legendpos=legendpos[1], legendsep=legendsep[1],
            legendstyle=legendstyle[1], tickpos=options.tickpos[1], xyticks=options.xyticks[1],xylabelsep=options.xylabelsep[1],
            labelcolor=options.labelcolor[1], node_options=options.xnode_options, exponent=options.xexponent,
            labelpos=options.labelpos[1], use_siunitx = options.use_siunitx[1], legendangle=legendangle[1],
            labelstyle=options.labelstyle[1], labelangle=options.labelangle[1]
            })
            
        options.nbsubdiv = options.nbsubdiv[2]
        options.unit = unit[2]
        options.legend = legend[2]; options.legendpos = legendpos[2]
        options.legendsep = legendsep[2]; options.legendstyle = legendstyle[2]
        options.legendangle = legendangle[2]
        
        options.tickpos = options.tickpos[2]; options.xyticks = options.xyticks[2]
        options.xylabelsep = options.xylabelsep[2]
        options.node_options = options.xnode_options
        options.labelcolor = options.labelcolor[2]
        options.labelden = options.labelden[2]
        options.labeltext = options.labeltext[2]
        options.labelstyle = options.labelstyle[2]
        options.labelangle = options.labelangle[2]
        options.labelpos = options.labelpos[2]
        options.use_siunitx = options.use_siunitx[2]
        self:DaxeY({Z(0,options.originloc.im),options.ystep}, options)    
    
    elseif logtype == "logy" then
        eps = (xmax-xmin)*eps
        grid = options.grid[2]
        options.showlines = {true, false}
        options.nbsubdiv = options.nbsubdiv or {0,3}
        options.limits = {xmin-eps,xmax+eps}
        options.gradlimits = {xmin,xmax}
        options.originloc = toComplex( options.originloc or Z(xmin,0) )
        options.xstep = options.xstep or 1
        options.labelpos = options.labelpos or {"bottom","left"} -- "none" or "right" or "left"
        local unit = options.unit or {"",""} 
        if options.grid[1] then
            options.unit = {options.xstep, 1}
            self:Dgrid({Z(options.originloc.re,0),Z(xmax+eps,decadey+1e-6)}, options)
        end        
        self:Dlogaxe("logy", {step=options.ystep, nbsubdiv=options.nbsubdiv[2], decadeloglabels=options.ydecadeloglabels,
            addloglabels=options.yaddloglabels, legend=legend[2], legendpos=legendpos[2], legendsep=legendsep[2],
            legendstyle=legendstyle[2], tickpos=options.tickpos[2], xyticks=options.xyticks[2],xylabelsep=options.xylabelsep[2],
            labelcolor=options.labelcolor[2], node_options=options.ynode_options, exponent=options.yexponent,
            labelpos=options.labelpos[2],use_siunitx = options.use_siunitx[2], legendangle=legendangle[2],
            labelstyle=options.labelstyle[2], labelangle=options.labelangle[2]
            })
            
        options.nbsubdiv = options.nbsubdiv[1]
        options.unit = unit[1]
        options.legend = legend[1]; options.legendpos = legendpos[1]
        options.legendsep = legendsep[1]; options.legendstyle = legendstyle[1]
        options.legendangle = legendangle[1]
        
        options.tickpos = options.tickpos[1]; options.xyticks = options.xyticks[1]
        options.xylabelsep = options.xylabelsep[1]
        options.node_options = options.xnode_options
        options.labelcolor = options.labelcolor[1]
        options.labelden = options.labelden[1]
        options.labeltext = options.labeltext[1]
        options.labelstyle = options.labelstyle[1]
        options.labelangle = options.labelangle[1]
        options.labelpos = options.labelpos[1]
        options.use_siunitx = options.use_siunitx[1]
        self:DaxeX({Z(options.originloc.re,0),options.xstep }, options)
        
    elseif logtype == "logxy" then
        options.nbsubdiv = options.nbsubdiv or {3,3}
        options.labelpos = options.labelpos or {"bottom","top"} -- "none" or "right" or "left"
        grid = options.grid[1]
        self:Dlogaxe("logx", {step=options.xstep, nbsubdiv=options.nbsubdiv[1], decadeloglabels=options.xdecadeloglabels,
            addloglabels=options.xaddloglabels, legend=legend[1], legendpos=legendpos[1], legendsep=legendsep[1],
            legendstyle=legendstyle[1], tickpos=options.tickpos[1], xyticks=options.xyticks[1],xylabelsep=options.xylabelsep[1],
            labelcolor=options.labelcolor[1], node_options=options.xnode_options, exponent=options.xexponent,
            labelpos=options.labelpos[1], use_siunitx = options.use_siunitx[1], legendangle=legendangle[1],
            labelstyle=options.labelstyle[1], labelangle=options.labelangle[1]
            })
        grid = options.grid[2]
        self:Dlogaxe("logy", {step=options.ystep, nbsubdiv=options.nbsubdiv[2], decadeloglabels=options.ydecadeloglabels,
            addloglabels=options.yaddloglabels, legend=legend[2], legendpos=legendpos[2], legendsep=legendsep[2],
            legendstyle=legendstyle[2], tickpos=options.tickpos[2], xyticks=options.xyticks[2],xylabelsep=options.xylabelsep[2],
            labelcolor=options.labelcolor[2], node_options=options.ynode_options, exponent=options.yexponent,
            labelpos=options.labelpos[2], use_siunitx = options.use_siunitx[2], legendangle=legendangle[2],
            labelstyle=options.labelstyle[2], labelangle=options.labelangle[2]
            })
    end
end

------------------------------------------------------------------------
function Zlog(z) -- a and b and real
-- returns the complex affix on the log grid
    z = toComplex(z)
    local a, b = z.re, z.im
    if logtype == "logx" then
        if a > 0 then return cpx:new(log(a)-powerminx, b) end
    elseif logtype == "logy" then
        if b > 0 then return cpx:new(a,log(b)-powerminy) end
    elseif logtype == "logxy" then
        if (a > 0) and (b > 0) then return cpx:new(log(a)-powerminx,log(b)-powerminy) end
    end
end

local conv2log = function (L)
-- L is list of complex numbers ora list of lists of complex numbers
-- returns L adapted for the log grid
    local conv
    if logtype == "logx" then
        conv = function(z)
            if type(z) == "number" then z = toComplex(z) end
            if isComplex(z) then 
                local a, b = z.re, z.im
                if a > 0 then return cpx:new(log(a)-powerminx, b) end
            else
                return z
            end
        end
    elseif logtype == "logy" then
        conv = function(z)
            if type(z) == "number" then z = toComplex(z) end
            if isComplex(z) then 
                local a, b = z.re, z.im
                if b > 0 then return cpx:new(a,log(b)-powerminy) end
            else
                return z
            end
        end
    elseif logtype == "logxy" then
        conv = function(z)
            if type(z) == "number" then z = toComplex(z) end
            if isComplex(z) then 
                local a, b = z.re, z.im
                if (a > 0) and (b > 0) then return cpx:new(log(a)-powerminx,log(b)-powerminy) end
            else
                return z
            end
        end
    end
    return ftransform(L,conv)

end

function graph:Dlogpolyline(L,close,draw_options)
    self:Dpolyline(conv2log(L),close,draw_options,clipbox)
end

function graph:Dlogdots(L,mark_options)
    local L1 = conv2log(L)
    local epsx, epsy = (xmax-xmin)*1e-7, (ymax-ymin)*1e-7
    if clip then
        local x1,x2,y1,y2 = table.unpack(clipbox)
        self:Ddots( clipdots(L1,x1-epsx,x2+epsx,y1-epsy,y2+epsy), mark_options)
    else
        self:Ddots(L1,mark_options)
    end
end
    
function graph:Dlogline(A,B,draw_options)
    local a, b = Zlog(A), Zlog(B)
    if clip then
        local x1,x2,y1,y2 = table.unpack(clipbox)
        self:Dseg(clipline({a,b-a},x1,x2,y1,y2),1,draw_options)
    else
        self:Dline(a,b,draw_options)
    end
end

function graph:Dloglabel(...)
    local argslst, texte, anchor, args = {}
    local n = select("#", ...)  -- Nombre total d'arguments
    for i = 1, n-2, 3 do   -- Pas de 3 (1,4,7...)
        texte, anchor, args = select(i, ...)  -- Récupère les 3 args
        if anchor ~= nil then 
            table.insert(argslst,texte); table.insert(argslst,Zlog(anchor)); table.insert(argslst,args)
        else
            print("Dloglabel Warning : the anchor point associated with the text "..texte.." is equal to nil")
        end
    end
    if #argslst > 0 then self:Dlabel(table.unpack(argslst)) end
end
