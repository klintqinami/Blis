
(* given a list of nodes, a function that returns the successors for a given
 * node, and a function to call when a cycle is detected, performs a topological
 * sort and returns the sorted list of nodes.
 *)
let tsort nodes succs cycle =
  let rec tsort' path visited = function
      [] -> visited
    | n :: nodes -> 
        if List.mem n path then
          (cycle (List.rev (n :: path)); visited)
        else
          let v' = if List.mem n visited then visited else
            n :: tsort' (n :: path) visited (succs n)
          in tsort' path v' nodes
  in
  tsort' [] [] nodes

