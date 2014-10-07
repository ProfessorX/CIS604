import networkx as nx
import numpy as np
import bisect
import math


class GraphProblem:
    def __init__(self, initial, goal, connections, locations=None, directed=False):
        self.initial = initial
        self.goal = goal
        self.locations = locations
        self.graph = nx.DiGraph() if directed else nx.Graph()
        for cityA, cityB, distance in connections:
            self.graph.add_edge(cityA, cityB, cost=distance)
    def successors(self, state):
        ## Exactly as defined in Lecture slides, 
        return [("go to %s" % city, connection['cost'], city) for city, connection in self.graph[state].items()]
    def goal_test(self, state):
        return state == self.goal


class PuzzleState:
    def __init__(self, matrix=None, goal=False, init=False, size=3):
        self.size = size
        if not matrix is None: ## 0 represents empty spot
            self.matrix = matrix
        else: ## defining init or goal state
            permutation = np.array(range(size*size))
            if init:
                random.shuffle(permutation)
            self.matrix = permutation.reshape((size, size))
    def successors(self):
        potentialMoves = [(-1,0,"down"), (1,0,"up"), (0,-1, "left"), (0,1, "right")]
        ## find empty spot (0)
        x0,y0 = np.array(np.where(self.matrix==0)).flatten()
        succs = []
        for xd, yd, move in potentialMoves:
            xm, ym = x0 + xd, y0 + yd # coordinates of tile to be moved
            # make sure the tile coordinates are not  out of bound 
            if 0 <= xm < self.size and 0 <= ym < self.size: 
                ## preparing the successor state matrix with the tile moved
                newMatrix = self.matrix.copy()
                movedTile = newMatrix[xm, ym]
                newMatrix[xm, ym] = 0                
                newMatrix[x0, y0] = movedTile
                action = ("move tile %s %s" % (movedTile, move), 1, PuzzleState(matrix=newMatrix))
                succs.append(action)
        return succs
    def __hash__(self):
        return hash(tuple(self.matrix.flatten()))
    def __eq__(self, other):
        return np.alltrue(other.matrix == self.matrix)
    def __repr__(self):
        return str(self.matrix)
            
    
class PuzzleProblem:
    def __init__(self, size=3): #size 3 means 3x3 field
        self.size = size        
        self.initial = PuzzleState(init=True, size=size)  ## init state is shuffled 
        self.goal = PuzzleState(goal=True, size=size)  ## goal state is unshuffled 
    def successors(self, state):
        return state.successors()
    def goal_test(self, state):
        return state == self.goal


class VacuumState:
    def __init__(self, dirtDistribution, robotPosition):
        self.dirtDistribution = dirtDistribution
        self.robotPosition = robotPosition
    def __repr__(self):
        return "[%s]" % "|".join(["%s%s"%(d, "*" if roomNr==self.robotPosition else " ") for roomNr, d in enumerate(self.dirtDistribution)])
        
class VacuumProblem:    ## Deterministic, Fully observable
    def __init__(self, initial):
        self.initial=initial
    def successors(self, state):
        actions = []
        if state.robotPosition < len(state.dirtDistribution)-1:
            actions.append(("goRight", 1, VacuumState(state.dirtDistribution, state.robotPosition+1)))
        if state.robotPosition > 0:
            actions.append(("goLeft", 1, VacuumState(state.dirtDistribution, state.robotPosition-1)))        
        if state.dirtDistribution[state.robotPosition] > 0: ## current position dirty
            dirt = state.dirtDistribution[:]
            dirt[state.robotPosition] = 0
            actions.append(("suck", 1, VacuumState(dirt, state.robotPosition)))
        return actions
    def goal_test(self, state):
        return sum(state.dirtDistribution) == 0


class Node:
    def __init__(self, state=None, parent=None, action=None, path_cost=0):
        self.state = state
        self.parent = parent
        self.action = action
        self.path_cost = path_cost        
    def getPath(self):
        """getting the path of parents up to the root"""
        currentNode = self
        path = [self]
        while currentNode.parent: ## stops when parent is None, ie root
            path.append(currentNode.parent)
            currentNode = currentNode.parent
        path.reverse() #from root to this node
        return path
    def expand(self, problem):
        return [Node(newState, self, action, self.path_cost+cost)
                for (action, cost, newState) in problem.successors(self.state)]
    
class FIFO:
    def __init__(self):
        self.list = []
    def push(self, item):
        self.list.insert(0, item)
    def pop(self):
        return self.list.pop()
    def isEmpty(self):
        return len(self.list) == 0

class LIFO:  ## fill out yourself! first question in the coding section
    def __init__(self):
        pass
    def push(self, item):
        pass
    def pop(self):
        pass
    def isEmpty(self):
        pass

class PriorityQueue:
    def __init__(self, f):
        self.list = []
        self.f = f
    def push(self, item):
        bisect.insort(self.list, (self.f(item), item))
    def pop(self):
        return self.list.pop(0)[1]
    def isEmpty(self):
        return len(self.list) == 0

def tree_search(problem, frontier):
    """Search through the successors of a problem to find a goal.
    The argument frontier should be the appropriate data structure.
    """
    frontier.push(Node(problem.initial))
    explorationHistory = []
    while not frontier.isEmpty():
        node = frontier.pop()
        explorationHistory.append(node)
        if problem.goal_test(node.state):
            return node, explorationHistory
        successors = node.expand(problem)
        for snode in successors:
                frontier.push(snode)


def graph_search(problem, frontier):
    """Search through the successors of a problem to find a goal.
    The argument frontier should be an empty queue.
    If two paths reach a state, only use the best one. [Fig. 3.18]"""
    closed = set() ## can store hashable objects, thats why we need to define a hash code for states
    # fill out yourself!!

    
def breadth_first_graph_search(problem):
    return graph_search(problem, FIFO())
def astar_graph_search(problem, f):
    return graph_search(problem, PriorityQueue(f))



if __name__ == "__main__":    
    connections = [('A', 'S', 140), ('A', 'Z', 75), ('A', 'T', 118), ('C', 'P', 138), ('C', 'R', 146), ('C', 'D', 120), ('B', 'P', 101),
                   ('B', 'U', 85), ('B', 'G', 90), ('B', 'F', 211), ('E', 'H', 86), ('D', 'M', 75), ('F', 'S', 99), ('I', 'V', 92),
                   ('I', 'N', 87), ('H', 'U', 98), ('L', 'M', 70), ('L', 'T', 111), ('O', 'S', 151), ('O', 'Z', 71), ('P', 'R', 97), ('R', 'S', 80), ('U', 'V', 142)]
    
    locations =     {'A': (91, 492), 'C': (253, 288), 'B': (400, 327), 'E': (562, 293), 'D': (165, 299), 'G': (375, 270), 'F': (305, 449),
                     'I': (473, 506), 'H': (534, 350), 'M': (168, 339), 'L': (165, 379), 'O': (131, 571), 'N': (406, 537), 'P': (320, 368),
                     'S': (207, 457), 'R': (233, 410), 'U': (456, 350), 'T': (94, 410), 'V': (509, 444), 'Z': (108, 531)}


    def euclidDistance(state):
        pass


    #print romania.successors('A') ## [('go to S', 140, 'S'), ('go to Z', 75, 'Z'), ('go to T', 118, 'T')]
    #solution = breadth_first_graph_search(romania)
    #print [(node.state, node.action) for node in solution.getPath()]
    #pylab.clf()
    #nx.draw(romania.graph)
    #pylab.show()

    #puzzle = PuzzleProblem()
    romania = GraphProblem('A', 'B', connections)

    ## Uniform cost:
    sol, history = graph_search(romania, PriorityQueue(lambda node: node.path_cost))
    print("Solution:", [(node.state, node.action) for node in sol.getPath()])
    print("exploration history:", [node.state for node in history])

    ## Best first
    sol, history = graph_search(romania, PriorityQueue(lambda node: euclidDistance(node.state)))
    print("Solution:", [(node.state, node.action) for node in sol.getPath()])
    print("exploration history:", [node.state for node in history])

    ## A*
    sol, history = graph_search(romania, PriorityQueue(lambda node: node.path_cost + euclidDistance(node.state)))
    print("Solution:", [(node.state, node.action) for node in sol.getPath()])
    print("exploration history:", [node.state for node in history])