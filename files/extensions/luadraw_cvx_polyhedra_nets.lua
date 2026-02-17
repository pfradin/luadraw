-- luadraw_polyhdron_net.lua (chargé par luadraw_graph2d.lua)
-- date 2026/02/17
-- version 2.6
-- Copyright 2026 Patrick Fradin
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt.

-- net of convex polyhedrons

local getpos = function(elt, L)
-- renvoie la position de elt dans la liste L
    for k,x in ipairs(L) do
        if x == elt then return k end
    end
end

local common_edge = function(F1,F2)
-- F1, F2 deux faces avec numéros de sommets, on renvoie la position dans F1 des sommets communs (F1 sera l'ancêtre de F2)
    local b, a = F2[1]
    local pos2, pos1 = getpos(b,F1)
    local n = #F2
    for k = 2, n+1 do
        a = b; b = F2[1+(k-1)%n]
        pos1 = pos2; pos2 = getpos(b,F1)
        if (pos1 ~= nil) and (pos2 ~= nil) then 
            return {pos1,pos2}
        end
    end
end

local test_model = function(F,model)
    -- F = facets (with vertices numbers), model = list of lists or facet numbers
    local visited = map(function(k) return false end, range(1,#F))
    local neighbor = function(F1,F2)
        local A = common_edge(F1,F2)
        return (A~= nil)
    end
    local a, b, F2, F1, A
    for _,cp in ipairs(model) do
        b = cp[1]; F2 = F[b]
        for k = 2, #cp do
            a = b; b = cp[k]; F1 = F2; F2 = F[b]
            A = common_edge(F1,F2)
            if A == nil then
                print("net model error : facets "..a.." and "..b.." are not adjacent, aborted.")
                return false
            end
            if visited[b] == true then
                print("net model error : facet "..b.." has two ancestors, aborted.")
                return false
            else
                visited[b] = true
            end
        end
    end
    local list_root = {}
    for k,f in ipairs(visited) do
        if f == false then table.insert(list_root,k) end
    end
    if #list_root ~= 1 then
        print("net model error : "..(#list_root).." facets without ancestor instead of one, aborted.")
        print("facets: ",table.unpack(list_root))
        return false
    end
    return true
end

function unfold_tree(tree, factor, num)
-- tree = { {ancestor, n1, n2, angle, vertices}, ...}
-- num est le numéro de facette (rang d'apparition dans P.facets)
    local rep = {}
    local arbre = table.copy(tree)
    if num ~= nil then -- on cherche le rang de cette facette dans l'arbre)
        for k, elt in ipairs(tree) do
            if num == elt[6] then num = k; break end
        end
    end
    factor = factor or 1
    for k, Z in ipairs(arbre) do
        local parent, a1,a2, angle, S, index = table.unpack(Z)
        angle = factor*angle
        if (num==nil) or (num==k) then -- on tourne tout ou bien la facette k et sa descendance
            if parent == 0 then 
                table.insert(rep,S) -- c'est la facette initiale (root), elle ne tourne pas
                --rep[index] = S
            else
                local F = arbre[parent][5] -- sommets du parent
                local axe = {F[a1], F[a2]-F[a1]}
                local S1 = rotate3d(S,angle,axe) -- rotation de la facette
                Z[5] = S1
                table.insert(rep, S1)
                --rep[index] = S1
                local j, ancestors = 0, {k}
                for _, Y in ipairs(arbre) do
                    j = j+1
                    if getpos(Y[1],ancestors) ~= nil then -- Y fait partie des descendants de Z
                        Y[5] = rotate3d(Y[5],angle,axe) -- on tourne Y
                        table.insert(ancestors,j)  --les descendants de Y doivent tourner eux aussi, ils sont après Y dans l'arbre
                    end
                    end
            end
        else table.insert(rep, S)
        end
    end
    return rep
end


unfold_polyhedron = function(P,options)
-- P = polyèdre, options = {opening= dans [0,1], root = n° facette, to2d=false, tabs=false, tabs_wd=0.2, tabs_lg=0.5, rotate=0}
-- renvoie le patron ouvert au taux d'ouverture voulu (entre 0 et 1), le patron est une liste de facettes avec sommets 2d ou 3d.
    options = options or {}
    local opening = options.opening or 1
    local root =  options.root or 1
    local to2d = options.to2d  or false
    local rotangle = options.rotate or 0
    local tabs = options.tabs  or false
    local out = {}
    local model = options.model
    if tabs then to2d = true end
    if to2d then opening = 1 end
    local tabs_wd = options.tabs_wd or 0.2
    local tabs_lg = options.tabs_lg or 0.5
    local lambda = (1-tabs_lg)/2
    
    local S = P.vertices
    local F = P.facets
    local nb_facet = #F
    local visited_facet = map(function() return false end, F)
    local tree_index = range(1,#F)
    local edges_list = {}
    local arbre
    
    local normal_vector = function(face)
    -- renvoie un vecteur normal à la face (face contient les numéros de sommets dans la liste S) 
        local A, B, C = S[face[1]], S[face[2]], S[face[3]]
        return pt3d.prod(B-A,C-A)
    end
    
    local neighbor = function(n, face, num)
    -- n est le vecteur normal à face
    -- ajoute à arbre les facettes adjacentes à face, num est le numéro de la face dans l'arbre
        local Z, A, N
        for k = 1, nb_facet do
            if not visited_facet[k] then
                Z = F[k]
                A = common_edge(face,Z)
                if A ~= nil then
                    N = normal_vector(Z)
                    table.insert(arbre, {num, A[1],A[2], angle3d(N,n)*rad, N, Z, k} ) -- on ajoute k, le numéro de la facette dans P
                    tree_index[k] = #arbre -- position de la face k dans l'arbre
                    visited_facet[k] = true
                end
            end
        end 
    end
    
    local facet2tree = function()
    -- P = polyedre convexe
    --conversion en arbre où arbre= { {numero parent, numero point commun 1, numero point commun 2, angle avec parent, vecteur normal, sommets}, ...}
        local face = F[root]
        local N = normal_vector(face)
        --if abs(%2) then viewDir(%2*N) fi,
        arbre = { {0,0,0,0, N, face, root} } -- facette de départ
        visited_facet[root] = true -- première facette en cours de visite
        tree_index[root] = 1
        local k = 1 -- index des faces dans la liste arbre
        while face ~= nil do
            neighbor(N,face,k) -- on ajoute les facettes adjacentes (qui sont dans F) dans l'arbre
            k = k+1 --facette suivante
            if k <= #arbre then
                local f = arbre[k]
                N = f[5] --  vecteur normal de la
                face = f[6] -- facette suivante 
            else face = nil -- on a parcouru arbre
            end
        end
        local rep = {}
        for _, Z in ipairs(arbre) do --on renvoie l'arbre sans les vecteurs normaux 
            local t =  { Z[1], Z[2], Z[3], Z[4], map(function(k) return S[k] end, Z[6]), Z[7]} -- {ancetre,axe1,axe2,angle,sommets, index initial}
            if out ~= nil  then
                table.insert(out, table.copy(t))
            end
            table.insert(rep,t)
        end
        arbre = rep
    end
    
    
    local model2tree = function()
        -- model = { {f1,f2,...fn}, {f2,f4,...},...} les fi sont des numéros de facettes, chaque facette est précédée de son ancêtre (qui doit être une facette adjacente) sauf la facette racine, chaque facette doit avoir exactement un ancêtre (sauf la facette racine). La fonction ne vérifie pas si ces conditions sont remplies, c'est au risque de l'utilisateur...
        local read_model = function()
            local rep = {}
            local visited = map(function(f) return false end, F)
            for _, L in ipairs(model) do
                local b, a = L[1]
                for k = 2, #L do
                    a = b; b = L[k]; 
                    if not visited[b] then table.insert(rep,{a,b}); visited[b] = true end
                end
            end
            for _, L in ipairs(model) do
                local a = L[1]
                if not visited[a] then table.insert(rep,1,{0,a}); root=a; break end -- root
            end
            return rep
        end
        if not test_model(F,model) then arbre = nil; return end
        model = read_model()
        -- mintenant model = { {0,i1}, {i2,i3}, ...} où les i1, i2, ... sont des n°s de facettes dans F par paires : {ancêtre, descendant}
        -- on fait un tri du modèle (les descendants d'une facette doivent être à droite)
        local visited = map( function(k) return false end, model)
        local a, d, m
        local rep = {}
        while #model > 0 do
            m = table.remove(model)
            a, d = table.unpack(m)
            if (a==0) or (visited[a]) then table.insert(rep,m); visited[d] = true
            else table.insert(model,1,m)
            end
        end
        model = rep
        local normals = map( function(f) return normal_vector(f) end, F)
        --local index_tree = map(function(k) return 0 end,model)
        arbre = {}
        for k, m in ipairs(model) do
            a,d = table.unpack(m)
            local Z, face, A, n = F[d]
            local N = normals[d]
            if a == 0 then  
                table.insert(arbre, {0,0,0,0,N,Z,d} )
                --index_tree[d] = #arbre
                tree_index[d] = #arbre -- position de la face k dans l'arbre
            else
                face = F[a]; n = normals[a]
                A = common_edge(face,Z)
                if A ~= nil then
                    table.insert(arbre, {a, A[1],A[2], angle3d(N,n)*rad, N, Z, d} )
                    --index_tree[d] = #arbre
                    tree_index[d] = #arbre -- position de la face k dans l'arbre
                else
                    print("net model error: facet "..a.." and facet "..d.." are not adjacent, aborted.\n")
                    arbre = nil; return
                end
            end
        end
        local rep = {}
        for m, Z in ipairs(arbre) do --on renvoie l'arbre sans les vecteurs normaux
            local index
            if Z[1] == 0 then index = 0 else index = tree_index[Z[1]] end
            local t =  {index, Z[2], Z[3], Z[4], map(function(k) return S[k] end, Z[6]), Z[7]} -- {ancetre,axe1,axe2,angle,sommets, index initial}
            if out ~= nil  then
                table.insert(out, table.copy(t))
            end
            table.insert(rep,t)
        end
        arbre = rep
    end
    -- corps de la fonction unfold_polyhedron
    --if root ~= 1 then P.facets[1], P.facets[root] = P.facets[root], P.facets[1]  end -- on met la facette de base en premier
    if model == nil then facet2tree() else model2tree() end -- conversion en arbre
    if arbre == nil then return end
    if opening == 0 then return poly2facet(P)  end -- rien à faire, on renvoie les facettes de P  
    local rep, tabs_list, twin_edges = {}
    rep = unfold_tree(arbre,opening) -- on ouvre le polyèdre
    if to2d then -- conversion en 2d, le patron est à plat dans le plan de l'écran
        local x1,x2,y1,y2,z1,z2 = getbounds3d(rep)
        local G = M((x1+x2)/2, (y1+y2)/2, (z1+z2)/2 )
        local P = facet2plane( rep[1] )
        local A, u, v = orthoframe({G,-P[2]})
        rep = ftransform3d( rep, function(N) return Z(pt3d.dot(u,N-A), pt3d.dot(v,N-A)) end)
        if rotangle ~= 0 then
            rep = rotate(rep,rotangle)
        end
        if tabs then
            tabs_list = {}
            twin_edges = {}
            local a, b, ab, n, id1, id2
            for num,facet in ipairs(F) do -- parcours du polyèdre par facette
                b = facet[1]; n = #facet; id2 = 1
                for k = 2, n+1 do
                    if k == n+1 then k = 1 end
                    a = b; b = facet[k]; id1 = id2; id2 = k --id = n° arête
                    if a > b then 
                        ab = a..";"..b -- pour servir de clé
                    else ab = b..";"..a
                    end
                    if edges_list[ab] == nil then edges_list[ab] = {id1,id2,tree_index[num]}
                    else insert(edges_list[ab], {id1,id2,tree_index[num]})
                    end
                end
            end
            for _,edge in pairs(edges_list) do
                -- edge est une liste {i1, i2} où i1 et i2 sont les indices dans arbre des deux facettes partagent l'arête
                if #edge == 6 then -- ce n'est pas une arête solitaire
                    local a1,b1,n1,a2,b2,n2 = table.unpack(edge)
                    if (arbre[n1][1] ~= n2) and (arbre[n2][1] ~= n1 ) then -- si une des deux facettes est l'ancêtre de l'autre, elles sont attachées par l'arête dans le patron, donc pas de languette sur cette arête                
                    local A, B, C, D = rep[n1][a1], rep[n1][b1], rep[n2][a2], rep[n2][b2]
                    -- [A,B] est une des deux arêtes qui se confondent quand le polyèdre est replié, mais elles sont différentes sur le patron alors il faut une languette sur une des deux
                        local h = tabs_wd*cpx.I*cpx.normalize(B-A)
                        table.insert(tabs_list, {A, (1-lambda)*A+lambda*B+h, lambda*A+(1-lambda)*B+h,B} )
                        table.insert(twin_edges,{{A,B},{C,D}})
                    end
                end
            end
        end
    elseif rotangle ~= 0 then
        local f = rep[1] -- root facet
        rep = rotate3d(rep, rotangle, {isobar3d(f), pt3d.prod(f[2]-f[1],f[3]-f[1])}) 
    end
    local rep2 = map(function(k) return {} end, range(1,#rep))
    for k,f in ipairs(arbre) do
        rep2[f[6]] = rep[k]
    end
    local bounds
    if to2d then
        bounds = {getbounds( concat(rep2,tabs_list) )}
    else
        bounds = {getbounds3d( rep2 )}
    end
    if bounds == nil then bounds= {} end
    local ret = {["facets"]=rep2, ["tree"]=out, ["bounds"]=bounds}
    if tabs then ret.tabs = tabs_list; ret.twins = twin_edges end
    return ret
end


function graph3d:Dpolyhedron_net(P,options)
-- draw the 2d or 3d version of net of P
    options = options or {}
    local to2d = options.to2d or false
    local tabs = options.tabs or false
    if tabs then to2d = true end
    -- only for 2d version
    local facet_name = options.facet_name or false
    local edge_name = options.edge_name or false
    local tabs_options = options.tabs_options or ""
    -- end
    local facet_options = options.facet_options
    local net = unfold_polyhedron(P, {tabs=tabs, to2d=to2d, opening=options.opening, root=options.root, model=options.model, tabs_wd=options.tabs_wd, tabs_lg=options.tabs_lg, rotate=options.rotate})
    if net == nil then return end
    local F = net.facets
    if to2d then
        self:Dpolyline(F,true,facet_options)
        if tabs then self:Dpolyline(net.tabs,false,tabs_options) end
        if edge_name then
            for k,f in ipairs(net.twins) do
                local a, b = unpack(f[1])
                local ab = (a+b)/2-0.1875*cpx.I*(b-a)/self:Abs(b-a)
                local c, d = unpack(f[2])
                local cd = (c+d)/2-0.1875*cpx.I*(d-c)/self:Abs(d-c)
                self:Dlabel("e"..k, ab, {node_options=" font=\\scriptsize"}, "e"..k, cd, {})
            end
        end
        if facet_name then
            for k,f in ipairs(F) do
                self:Dlabel("F"..k, isobar(F[k]), {node_options="font=\\footnotesize"})
            end
        end
        print("bounds of net =", table.unpack(net.bounds))
    else --3d version
       self:Dfacet(F,facet_options) 
       local x1,x2,y1,y2,z1,z2 = table.unpack(net.bounds)
       print("bounds of net =",getbounds( self:Proj3d( parallelep(M(x1,y1,z1),(x2-x1)*vecI, (y2-y1)*vecJ, (z2-z1)*vecK).vertices) ) )
    end
    
end
