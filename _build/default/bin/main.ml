open Graphics

let a, p = 200, 3
let board, colors = Array.make_matrix a a (-1), [|black; white|]

let turn x y c =
    (*donne à la cellule en x y la couleur c et la redessine*)
    if board.(x).(y) <> c then
        (set_color colors.(c);
        fill_rect (x * p) (y * p) (p-1) (p-1));
    board.(x).(y) <- c

let randomize_board () =
    Random.self_init();
    for x = 0 to a - 1 do for y = 0 to a - 1 do
        turn x y (Random.int (Array.length colors))
    done; done

let count_around tab x0 y0 r centre_exclu =
    (*rend le tableau v qui donne 
    en position k le nombre de fois qu'apparait la couleur k 
    dans le voisinage carré de la cellule en x y qui l'exclue, de rayon r*)
    let v = Array.make (Array.length colors) 0 in
    for j = -r to r do 
        let y = (y0 + j + a) mod a in
        for i = -r to r do
            let x = (x0 + i + a) mod a in
            if not ((x, y) = (x0, y0) && centre_exclu) then ( 
                let c = tab.(x).(y) in
                v.(c) <- v.(c) + 1)
        done; 
    done;    
    v

let rec loop () = 
    let prev = Array.init a (fun i -> Array.copy board.(i)) in (*les couleurs de l'étape precédente*)
    for x = 0 to a - 1 do for y = 0 to a - 1 do
        
        
        (*règles du jeu de la vie de Conway, 2 couleurs*)
        let v = count_around prev x y 1 true in
        turn x y (match prev.(x).(y) with
            | 1 -> if v.(1) = 2 || v.(1) = 3 then 1 else 0
            | 0 -> if v.(1) = 3 then 1 else 0
            | _ -> failwith "Fausse couleur")
        
    done; done;
    if not (key_pressed ()) then loop ()

let () = 
    open_graph " "; resize_window (p * a) (p * a);
    randomize_board ();
    loop ()