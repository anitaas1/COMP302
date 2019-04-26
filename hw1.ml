let mysqrt (x:float) = 
  let rec helper1 x g = 
    let g2 = square (g) 
    in 
    if close (x, g2) then g
    else let h = (g +. (x /. g)) /. 2.0
      in 
      if close(x, square(h)) then h
      else helper1 x h
  in 
  helper1 x 1.0
        
    


let cube_root (x:float) = 
  let rec helper2 x g = 
    let g3 = cube (g)
    in 
    if close(x, g3) then g
    else let h = ((2.0 *. g) +. (x /. (square (g)))) /. 3.0 
      in 
      if close(x, cube (h)) then h
      else helper2 x h 
  in 
  helper2 x 1.0 

    


let fast_exp (base, power) = 
  let rec helper_rpe (tmp, base, power)  = 
    if base = 0 then 0 
    else if power = 0 then tmp 
    else if (odd power) then helper_rpe((base * tmp), (base * base), (power - 1)/2) 
    else helper_rpe(tmp, (base * base) , (power/2)) 
  in 
  helper_rpe (1, base, power)  
                                         

                           

