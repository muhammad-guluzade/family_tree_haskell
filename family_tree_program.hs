
---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------


--declaring data structure

--the data structure defined below has 2 possibilities:
--	1. empty tree
--	2. a tree where name :: [Char], children number :: Int, years lived :: (Int, Int), and children :: [FamilyTree] are stored
--how it works
--	if a person has no children
--	  he/she will have a list of children with 1 element : EmptyTree
--	if a person has 1 or more children
--	  he/she will have a list of children with as many elements as his defined children number


data FamilyTree = EmptyTree | Node [Char] Integer (Integer,Integer) [FamilyTree] deriving(Show, Ord, Eq)


---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------


--creating a tree

--the following part of the code contains 2 functions : familytree, helper

--familytree
--	description
--    this function will receive a list of names, list of children numbers, and list of years alive
--    in order to return a family tree other than B. It uses "helper" function, because our
--    first node has to be a tree, and all of the remaining nodes have to be in a list. So, as it is
--    impossible for one function to return both tree and list of trees, I created "helper" function.
--  inputs : names list, children number list, years lived list
--	outputs : a family tree of whatever family is entered using 3 lists 

--helper
--  description
--    this function receives a list of names, list of children numbers, and list of years lived, and it has
--    one extra variable, which is "x". Variable "x" controls the number of children if it is not 0.
--    For example, if "Nikolaus1" has 3 children, x will control and make sure that the list of children
--    belonging to "Nikolaus1" actually is of length 3. After it is done, it returns a complete list of
--    Bernoulli family with correct children and number of children for each parent.
--  inputs : names list, children number list, years lived list, children number of previous parent
--  outputs : a family tree

familytree [] [] [] = EmptyTree
familytree (name:names) (child_num:children) (year:years) = (Node name child_num year (helper names children years child_num))

helper [] [] [] x = []
helper (name:names) (child_num:children) (year:years) x =
	if x > 1 && child_num == 0
	then [Node name child_num year [EmptyTree]]++(helper names children years (x-1))
	else if x > 1 && child_num == 1
	then [Node name child_num year (helper names children years (child_num))]++(helper (tail names) (tail children) (tail years) (head (tail children)))
	else if x > 0 && child_num /= 0
	then [Node name child_num year (helper names children years (child_num))]
	else [Node name child_num year [EmptyTree]]


---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------


--searching for a person

--to search for a person in a family tree, there are 2 functions : search, names_list_familytree

--search
--  description
--    this function just checks whether it is true that the name that is searched by the user
--    is in the list that is created by "names_list_familytree" function. 
--  inputs : family tree, searched name
--  outputs : True/False

--names_list_familytree
--  description
--    this function takes the tree list as an input. (This is the reason why in "search" function,
--    it is called as [tree]). It then traces the entire family tree and simply creates a list of
--    names found in all of the nodes of the tree.
--  inputs : family tree list
--  outputs : all of the names from family tree

search tree name = name `elem` names_list_familytree([tree])

names_list_familytree [] = []
names_list_familytree [EmptyTree] = []
names_list_familytree ((Node name child_num year tree_list):tree) = [name]++names_list_familytree(tree)++names_list_familytree(tree_list)


---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------


--finding a parent of a person

--to find a parent of a person, there are 4 functions : parent, parent_helper, lister, names_list_familytree

--parent
--  description
--    it takes the tree and checks its root. By definition, a root cannot have a parent. So, if the name
--    entered by the user is the name of the root, this parent returns "No parent". Also, by definition,
--    every member of the family tree except root node will have a parent. According to this fact,
--    we can say that if the name entered by the user is not inside the list of all names in the family
--    tree, we can definitely say that the parent is not found, since the member with the entered name does
--    not exist. However, if the name is inside of the list which contains all names of the family tree
--    then the member with that name is definitely going to have parent, so we proceed with parent search. 
--  inputs : family tree, searched_name
--  outputs : name of the parent/"Not found"/"No parent"

--lister
--  description
--    this function is similar to "names_list_familytree" function, but it creates a list of names of the given
--    level of the tree. This is needed to check every children list of every member to check whether the person
--    whose name was entered by the user in "parent" function is inside of that list. If it is, we conclude that
--    the current node is the parent and return the name.
--  inputs : family tree list
--  outputs : list of names at a particular level in the family tree

--parent_helper
--  description
--    this is the main recursive function that is going to trace the entire family tree to find the parent of the
--    person whose name was entered. If it finds the parent, it returns parent's name, if it does not, it returns
--    empty list, since "Not found" and "No parent" cases are already treated inside "parent" function.
--  inputs : family tree list, name searched
--  outputs : parent name

--names_list_familytree
--  refer to line 83

parent (Node name child_num year children) searched_name = 
	if (not (searched_name `elem` (names_list_familytree [Node name child_num year children])))
	then "Not found"
	else if name==searched_name
	then "No parent"
	else (parent_helper children searched_name)

parent_helper [] searched_name = []
parent_helper [EmptyTree] searched_name = []
parent_helper ((Node name child_num year children):tree) searched_name = 
	if searched_name `elem` (lister children)
	then name
	else []++(parent_helper children searched_name)++(parent_helper tree searched_name)

lister [] = []
lister [EmptyTree] = []
lister ((Node name child_num year tree_list):tree) = [name]++lister tree


---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------


--finding a sibling of a person

--to find a sibling, there are 4 functions : sibling, sibling_helper, print_siblings, lister

--sibling
--  description
--    this one is created only because the actual implementation requires a list input, but our tree is FamilyTree,
--    not [FamilyTree]. Therefore, it serves to give the inputs to the function that actually searches for siblings.
--  inputs : family tree, searched name

--sibling_helper
--  description
--    this function checks whether the entered name is the element of list of names of certain list of children.
--    It checks it using lister function, which creates a list of names of a certain tree level. If the name is 
--    found, it calls "print_siblings" function, which is declared only because we need to exclude the name of 
--    the person searched from the list of his/her siblings. If the sibling is not found, it continues tracing
--    the entire tree, and returns empty list if no sibling is found.
--  inputs : family tree list, searched name
--  outputs : list of siblings/empty list

--print_siblings
--  description
--    takes the list which is supposed to contain the names of siblings of searched person. It then creates
--    a list of names of those siblings, without the name of the person searched, as required by the assignment.
--  inputs : family tree list at a certain level, searched name
--  outputs : siblings list of the searched name

--lister
--  refer to line 119

sibling (Node name child_num year children) searched_name = (sibling_helper [Node name child_num year children] searched_name)

sibling_helper [] searched_name = []
sibling_helper [EmptyTree] searched_name = []
sibling_helper ((Node name child_num year children):tree) searched_name = 
	if searched_name `elem` (lister children)
	then (print_siblings (lister children) searched_name)
	else []++(sibling_helper children searched_name)++(sibling_helper tree searched_name)

print_siblings [] found_one = []
print_siblings (sibling_name:sibling_names) found_one = 
	if sibling_name==found_one
	then []++(print_siblings sibling_names found_one)
	else [sibling_name]++(print_siblings sibling_names found_one)


---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------


--calculating average lifetime

--to calculate average lifetime, there are 2 functions : avglifetime, ages_list_familytree

--avglifetime
--  description
--    it takes the sum of the ages of all members of the family tree and divides by the number of members in order to
--    receive average lifetime.
--  inputs : family tree
--  outputs : average lifetime of the familytree

--ages_list_familytree
--  description
--    it takes the family tree as a list in order to trace it and create a list of ages of each family member. 
--  inputs : family tree list
--  outputs : list of ages for each member of the family tree

avglifetime tree = (fromIntegral (sum (ages_list_familytree [tree]))) / (fromIntegral (length (ages_list_familytree [tree])))

ages_list_familytree [] = []
ages_list_familytree [EmptyTree] = []
ages_list_familytree ((Node name child_num year tree_list):tree) = [snd(year)-fst(year)]++ages_list_familytree(tree)++ages_list_familytree(tree_list)


---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------


--functions which represent input

--the following functions represent the input that was given in the assignment.
--they are necessary because familytree function uses them as an input in order to create family tree of Bernoulli family.

names =  ["Nikolaus1", "Jacob I", "Nikolaus2", "Nikolaus I", "Johann I", "Nikolaus II", "Daniel", "Johann II", "Johann III", "Jacob II"]

children = [3, 0, 1, 0, 3, 0, 0, 2, 0, 0]

years = [(1623, 1708), (1654, 1705), (1662, 1716), (1687, 1759), (1667, 1748), (1695, 1726), (1700, 1782), (1710, 1790), (1746, 1807), (1759, 1789)]
