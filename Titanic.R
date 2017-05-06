# titanic is avaliable in your workspace
> 
  > # Check out the structure of titanic
  > 
  > 
  > # Use ggplot() for the first instruction
  > 
  > 
  > 
  > # Use ggplot() for the second instruction
  > 
  > 
  > # Position jitter (use below)
  > posn.j <- position_jitter(0.5, 0)
  > 
    > # Use ggplot() for the last instruction
    > 
    > 
    > # titanic is avaliable in your workspace
    > 
    > # Check out the structure of titanic
    > 
    > 
    > # Use ggplot() for the first instruction
    > 
    > 
    > 
    > # Use ggplot() for the second instruction
    > 
    > 
    > # Position jitter (use below)
    > posn.j <- position_jitter(0.5, 0)
    > 
      > # Use ggplot() for the last instruction
      > 
      > 
      
      Parsing error in script.R:7:37: unexpected symbol
    6: # Use ggplot() for the first instruction
      7: ggplot(titanic, aes(x=factor(Pclass)fill
                             ^
                               
                               Parsing error in script.R:7:37: unexpected symbol
                             6: # Use ggplot() for the first instruction
                               7: ggplot(titanic, aes(x=factor(Pclass)fill
                                                      ^
                                                        
                                                        Parsing error in script.R:11:1: unexpected symbol
                                                      10: # Use ggplot() for the second instruction
                                                        11: ggplot
                                                      ^
                                                        > # titanic is avaliable in your workspace
                                                        > 
                                                        > # Check out the structure of titanic
                                                        > str(titanic)
                                                      'data.frame':	714 obs. of  4 variables:
                                                        $ Survived: int  0 1 1 1 0 0 0 1 1 1 ...
                                                      $ Pclass  : int  3 1 3 1 3 1 3 3 2 3 ...
                                                      $ Sex     : chr  "male" "female" "female" "female" ...
                                                      $ Age     : num  22 38 26 35 35 54 2 27 14 4 ...
                                                      > 
                                                        > # Use ggplot() for the first instruction
                                                        > ggplot(titanic, aes(x=factor(Pclass),fill=factor(Sex)))+ geom_bar(position="dodge")
                                                      > 
                                                        > 
                                                        > # Use ggplot() for the second instruction
                                                        > ggplot(titanic, aes(x=factor(Pclass),fill=factor(Sex))) + geom_bar(position="dodge") + facet_grid(". ~ Survived")
                                                      > 
                                                        > # Position jitter (use below)
                                                        > posn.j <- position_jitter(0.5, 0)
                                                      > 
                                                        > # Use ggplot() for the last instruction
                                                        > ggplot(titanic, aes(x=factor(Pclass), y=Age, col=factor(Sex))) + geom_jitter(position=posn.j, colour="blue",size=3, alpha=0.5) + facet_grid(". ~ Survived")
                                                      > 
                                                        > # Read This - Learned about adding a facet_grid to a plot, using a defined object (position posn.j). Need to learn more about facets and more about the best place to add aesthetics and attributes to get what I want without so many attempts. Note to self - Remember to change R option to more easily view the parentheses to prevent errors. Aaauuggghhhh!
                                                        > 