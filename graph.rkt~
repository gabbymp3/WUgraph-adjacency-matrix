#lang dssl2

# HW4: Graph



let eight_principles = ['Know your rights.',
                        'Acknowledge your sources.',
                        'Protect your work.',
                        'Avoid suspicion.',
                        'Do your own work.',
                        'Never falsify a record or permit another person to do so.',
                        'Never fabricate data, citations, or experimental results.',
                        'Always tell the truth when discussing your work with your instructor.']





import cons
import 'hw4-lib/dictionaries.rkt'


###
### REPRESENTATION
###

# A Vertex is a natural number.
let Vertex? = nat?

# A VertexList is either
#  - None, or
#  - cons(v, vs), where v is a Vertex and vs is a VertexList
let VertexList? = Cons.ListC[Vertex?]

# A Weight is a real number. (It’s a number, but it’s neither infinite
# nor not-a-number.)
let Weight? = AndC(num?, NotC(OrC(inf, -inf, nan)))

# An OptWeight is either
# - a Weight, or
# - None
let OptWeight? = OrC(Weight?, NoneC)

# A WEdge is WEdge(Vertex, Vertex, Weight)
struct WEdge:
    let u: Vertex?
    let v: Vertex?
    let w: Weight?

# A WEdgeList is either
#  - None, or
#  - cons(w, ws), where w is a WEdge and ws is a WEdgeList
let WEdgeList? = Cons.ListC[WEdge?]

# A weighted, undirected graph ADT.
interface WUGRAPH:

    # Returns the number of vertices in the graph. (The vertices are
    # numbered 0, 1, ..., k - 1.)
    def n_vertices(self) -> nat?

    # Returns the number of edges in the graph.
    def n_edges(self) -> nat?

    # Sets the weight of the edge between u and v to be w. Passing a
    # real number for w updates or adds the edge to have that weight,
    # whereas providing providing None for w removes the edge if
    # present. (In other words, this operation is idempotent.)
    def set_edge(self, u: Vertex?, v: Vertex?, w: OptWeight?) -> NoneC

    # Gets the weight of the edge between u and v, or None if there
    # is no such edge.
    def get_edge(self, u: Vertex?, v: Vertex?) -> OptWeight?

    # Gets a list of all vertices adjacent to v. (The order of the
    # list is unspecified.)
    def get_adjacent(self, v: Vertex?) -> VertexList?

    # Gets a list of all edges in the graph, in an unspecified order.
    # This list only includes one direction for each edge. For
    # example, if there is an edge of weight 10 between vertices
    # 1 and 3, then exactly one of WEdge(1, 3, 10) or WEdge(3, 1, 10)
    # will be in the result list, but not both.
    def get_all_edges(self) -> WEdgeList?
    
    
    
# pair struct
struct pair:
    let i: nat?
    let j: nat?
    
    


class WUGraph (WUGRAPH):
    
    let n_vert
    let adj_matrix
    let vlist
    let edge_count

    def __init__(self, n_vert: nat?):
        self.n_vert = n_vert
        let adj_matrix = [[None;n_vert] for i in range(n_vert)]
        self.adj_matrix = adj_matrix
        let vlist = None
        self.vlist = vlist
        let edge_count = 0
        self.edge_count = edge_count

        
        
    def n_vertices(self) -> nat?:
        return self.n_vert
        
        
    def n_edges(self) -> nat?: # MAKE O(1)!
        return self.edge_count

        
        
    def set_edge(self, u:Vertex?, v:Vertex?, w: OptWeight?) -> NoneC:
        let prev_val = self.adj_matrix[u][v]
        # replace value
        self.adj_matrix[u][v] = w
        self.adj_matrix[v][u] = w
        # update edge count accordingly
        if prev_val == None and w != None:
            self.edge_count = self.edge_count + 1
        elif prev_val != None and w == None:
            self.edge_count = self.edge_count - 1
        else:
            return None
        return None
        
        
    def get_edge(self, u: Vertex?, v: Vertex?) -> OptWeight?:
        return self.adj_matrix[u][v]

        
        
    def get_adjacent(self, v:Vertex?) -> VertexList?:
        let adjs = None
        for i in range(self.n_vert):
            if self.adj_matrix[v][i] != None:
                adjs = cons(i,adjs)
        return adjs
        
   
 #######--- Trying linked list.
    def get_all_edges(self) -> WEdgeList?:
        let count = 0
        let pairs = [None; 10*self.n_vertices()]
        let edges = [None; 10*self.n_vertices()]
        
        # in_pairs - check if edge between a vertex pair has already been counted.
        def in_pairs(pair):
            for element in pairs:
                if element == pair:
                    return True
            return False
        
        
        for i in range(self.n_vert):
            #println('%p', i)
            #println('Looking at row ^')
            for j in range(self.n_vert):
                #println('%p', j)
                #println('Looking at col ^')
                
                let curr_pair = pair(i,j)
                let opp_curr_pair = pair(j,i)
                
                
                if self.adj_matrix[i][j] != None:
                    #println('%p', curr_pair)
                    #println('^ pair being counted')
                    if in_pairs(opp_curr_pair):
                        #println('pair already found.')
                        pass
                        
                        
                    else:
                        #let old_vlist = self.vlist
                        #println('%p', old_vlist)
                        #self.vlist = cons(WEdge(i,j,self.adj_matrix[i][j]), old_vlist)
                        #println('pair not already found')
                        edges[count] = WEdge(i, j, self.adj_matrix[i][j])
                        pairs[count] = pair(i, j)
                        count = count + 1
                        
        let edge_count = 0
        for edge in edges:
            if edge != None:
                #println('%p', edge)
                #println('counted ^')
                edge_count = edge_count + 1
                
        let new_edges = [None;edge_count]
        for i in range(edge_count):
            new_edges[i] = edges[i]
            #println('%p', new_edges[i])
            #println('Edge above counted!')
            
        let edge_list = None
        for i in range(edge_count):
            edge_list = cons(new_edges[i],edge_list)
                        
        return edge_list
        
        
        
        '''
    def get_all_edges1(self) -> WEdgeList?:
        let count = 0
        #let pairs = [None; 2*self.n_vertices()]
        
        # in_pairs - check if edge between a vertex pair has already been counted.
        def in_pairs(pair):
            for element in pairs:
                if element == pair:
                    return True
            return False
        
        
        for i in range(self.n_vert):
            for j in range(self.n_vert):
                let curr_pair = pair(i,j)
                let opp_curr_pair = pair(j,i)
                if self.adj_matrix[i][j] != None:
                    if in_pairs(opp_curr_pair):
                        break
                    else:
                        #pairs[count] = pair(i, j, self.adj_matrix[i][j])
                        count = count + 1
                        
        return [pair for pair in pairs if pair != None]

        '''
        
    def print_matrix(self):
        println('%p', self.adj_matrix)
        


###
### List helpers
###

# To test methods that return lists with elements in an unspecified
# order, you can use these functions for sorting. Sorting these lists
# will put their elements in a predictable order, order which you can
# use when writing down the expected result part of your tests.

# sort_vertices : ListOf[Vertex] -> ListOf[Vertex]
# Sorts a list of numbers.
def sort_vertices(lst: Cons.list?) -> Cons.list?:
    def vertex_lt?(u, v): return u < v
    return Cons.sort[Vertex?](vertex_lt?, lst)

# sort_edges : ListOf[WEdge] -> ListOf[WEdge]
# Sorts a list of weighted edges, lexicographically
# ASSUMPTION: There's no need to compare weights because
# the same edge can’t appear with different weights.
def sort_edges(lst: Cons.list?) -> Cons.list?:
    def edge_lt?(e1, e2):
        return e1.u < e2.u or (e1.u == e2.u and e1.v < e2.v)
    return Cons.sort[WEdge?](edge_lt?, lst)

    
    
    
test 'init test':
    let w = WUGraph(3)
    #w.print_matrix()
    assert w.n_vertices() == 3
    w.set_edge(1,2,8)
    #w.print_matrix()
    assert w.get_edge(2,1) == 8
    assert w.get_edge(1,2) == 8
    assert w.n_edges() == 1
    w.set_edge(1,0,2)
    assert w.n_edges() == 2
    #w.print_matrix()
    println('%p', w.get_adjacent(1))


test 'n_edges test':
    let w = WUGraph(10)
    w.set_edge(0,2,5)
    w.set_edge(2,0,5)
    #w.print_matrix()
    assert w.n_edges() == 1
    w.set_edge(2,0,4)
    assert w.n_edges() == 1
    assert w.get_edge(2,0) == 4
    assert w.get_edge(0,2) == 4
    #w.print_matrix()
    #println('%p', sort_edges(w.get_all_edges()))
    w.set_edge(2,1,10)
    w.set_edge(1,2,10)
    #println('%p', sort_edges(w.get_all_edges()))
    assert w.n_edges() == 2
    
test 'n_edges update properly, n_vertices constant test':
    let w = WUGraph(5)
    assert w.n_vertices() == 5
    assert w.n_edges() == 0
    w.set_edge(0,1,9)
    assert w.n_edges() == 1
    assert w.n_vertices() == 5
    w.set_edge(0,1,None)
    assert w.n_edges() == 0
    
    
    
    
test 'set_edge and get_edge update properly test':
    let w = WUGraph(6)
    w.set_edge(0,1,8)
    assert w.get_edge(1,0) == 8
    assert w.get_edge(0,1) == 8
    w.set_edge(1,0,4)
    assert w.get_edge(1,0) == 4
    assert w.get_edge(0,1) == 4
    w.set_edge(0,1,None)
    assert w.get_edge(0,1) == None
    assert w.get_edge(1,0) == None
    
test 'get_adjacent test':
    let w = WUGraph(10)
    w.set_edge(0,1,8)
    w.set_edge(1,2,8)
    #println('%p', w.get_adjacent(1))
    #println('%p', w.get_adjacent(0))
    #println('%p', w.get_adjacent(2))
    w.set_edge(1,2,None)
    #println('%p', w.get_adjacent(1))
    #println('%p', w.get_adjacent(0))
    #println('%p', w.get_adjacent(2))
    
    
    
test 'get_all_edges test':
    let w = WUGraph(6)
    w.set_edge(0,0,8)
    w.set_edge(0,1,8)
    w.set_edge(1,3,8)
    w.set_edge(1,0,6) #update
    #println('%p', w.get_all_edges())
    
  
    
    
 
    
    
 #--------------------------------------------------------------------   
    
    
    
###
### BUILDING GRAPHS
###

def example_graph() -> WUGraph?:
    let result = WUGraph(6) # 6-vertex graph from the assignment
    result.set_edge(0,1,12)
    result.set_edge(1,2,31)
    result.set_edge(1,3,56)
    result.set_edge(2,4,-2)
    result.set_edge(5,2,7)
    result.set_edge(3,4,9)
    result.set_edge(3,5,1)
    return result
#   ^ ADD YOUR WORK HERE
    
#example_graph().print_matrix()

struct CityMap:
    let graph
    let city_name_to_node_id
    let node_id_to_city_name

def my_home_region():
    
    let city_to_node_dict = AssociationList()
    city_to_node_dict.put('Westport', 0)
    city_to_node_dict.put('Norwalk', 1)
    city_to_node_dict.put('Fairfield', 2)
    city_to_node_dict.put('Bridgeport', 3)
    city_to_node_dict.put('Stamford', 4)
    
    let node_to_city_dict = AssociationList()
    node_to_city_dict.put(0, 'Westport')
    node_to_city_dict.put(1, 'Norwalk')
    node_to_city_dict.put(2, 'Fairfield')
    node_to_city_dict.put(3, 'Bridgeport')
    node_to_city_dict.put(4, 'Stamford')
    
    let home_graph = WUGraph(5)
    home_graph.set_edge(0,1,5)
    home_graph.set_edge(0,2,10)
    home_graph.set_edge(0,3,15)
    home_graph.set_edge(0,4,12)
    home_graph.set_edge(1,2,15)
    home_graph.set_edge(1,3,20)
    home_graph.set_edge(1,4,7)
    home_graph.set_edge(2,3,5)
    
    let my_home_graph = CityMap(home_graph, city_to_node_dict, node_to_city_dict)
    return my_home_graph
    
    
    
test 'home graph test':
    let g = my_home_region()
    assert g.graph.n_edges() == 8
    assert g.graph.n_vertices() == 5
    assert g.graph.get_edge(0,1) == 5
    assert g.graph.get_edge(0,2) == 10
    assert g.graph.get_edge(3,0) == 15
    assert g.graph.get_edge(4,0) == 12
    assert g.graph.get_edge(2,1) == 15
    assert g.graph.get_edge(3,1) == 20
    assert g.graph.get_edge(4,1) == 7
    assert g.graph.get_edge(3,2) == 5
    #println('get all edges')
    #println('%p', g.graph.get_all_edges())
    #println('get adjacent')
    #println('%p', g.graph.get_adjacent(0))
    assert g.city_name_to_node_id.get('Westport') == 0
    assert g.city_name_to_node_id.get('Norwalk') == 1
    assert g.city_name_to_node_id.get('Fairfield') == 2
    assert g.city_name_to_node_id.get('Bridgeport') == 3
    assert g.city_name_to_node_id.get('Stamford') == 4
    assert g.node_id_to_city_name.get(0) == 'Westport'
    assert g.node_id_to_city_name.get(1) == 'Norwalk'
    assert g.node_id_to_city_name.get(2) == 'Fairfield'
    assert g.node_id_to_city_name.get(3) == 'Bridgeport'
    assert g.node_id_to_city_name.get(4) == 'Stamford'
    assert_error g.city_name_to_node_id.get('Branchburg')
    assert_error g.node_id_to_city_name.get(7)
    
    
    
    
    
    
    
    
    


###
### DFS
###

# dfs : WUGRAPH Vertex [Vertex -> any] -> None
# Performs a depth-first search starting at `start`, applying `f`
# to each vertex once as it is discovered by the search.
def dfs(graph: WUGRAPH!, start: Vertex?, f: FunC[Vertex?, AnyC]) -> NoneC:
    let seen = [-1; graph.n_vertices()]
    
    def traverse(w):
        if seen[w] == -1:
            seen[w] = True
            f(w)
            #println('%p', w)
            
            let neighbors = graph.get_adjacent(w)
            while neighbors is not None:
                traverse(neighbors.data)
                neighbors = neighbors.next

    return traverse(start)

            


# dfs_to_list : WUGRAPH Vertex -> ListOf[Vertex]
# Performs a depth-first search starting at `start` and returns a
# list of all reachable vertices.
#
# This function uses your `dfs` function to build a list in the
# order of the search. It will pass the test below if your dfs visits
# each reachable vertex once, regardless of the order in which it calls
# `f` on them. However, you should test it more thoroughly than that
# to make sure it is calling `f` (and thus exploring the graph) in
# a correct order.
def dfs_to_list(graph: WUGRAPH!, start: Vertex?) -> VertexList?:
    let list = None
    # Add to the front when we visit a node
    dfs(graph, start, lambda new: list = cons(new, list))
    # Reverse to the get elements in visiting order.
    return Cons.rev(list)

###
### TESTING
###

## You should test your code thoroughly. Here is one test to get you started:

test 'dfs_to_list(example_graph())':
    # Cons.from_vec is a convenience function from the `cons` library that
    # allows you to write a vector (using the nice vector syntax), and get
    # a linked list with the same elements.
    assert sort_vertices(dfs_to_list(example_graph(), 0)) \
        == Cons.from_vec([0, 1, 2, 3, 4, 5])
        
        

            
test 'graph with cycle':
    let cyclic_graph = WUGraph(5)
    cyclic_graph.set_edge(0,1,1)
    cyclic_graph.set_edge(0,4,1)
    cyclic_graph.set_edge(1,3,1)
    cyclic_graph.set_edge(4,3,1)
    cyclic_graph.set_edge(1,2,1)
    assert sort_vertices(dfs_to_list(cyclic_graph,0)) \
        == Cons.from_vec([0,1,2,3,4])
    assert sort_vertices(dfs_to_list(cyclic_graph, 3)) \
        == Cons.from_vec([0,1,2,3,4])
        
        
test 'disconnected graph':
    let dis_graph = WUGraph(4)
    dis_graph.set_edge(0,1,1)
    dis_graph.set_edge(2,3,1)
    assert sort_vertices(dfs_to_list(dis_graph, 0)) \ 
        == Cons.from_vec([0,1])
    assert sort_vertices(dfs_to_list(dis_graph,3)) \
        == Cons.from_vec([2,3])
        
        
        
test 'home region graph test':
    assert sort_vertices(dfs_to_list(my_home_region().graph, 0)) \ 
        == Cons.from_vec([0,1,2,3,4])
