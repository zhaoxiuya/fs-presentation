from manim import *

class SquareToCircle(Scene):
    def construct(self):
        square = Square(color=BLUE, fill_opacity=0.5)
        self.play(Create(square))
        self.wait(1)

        circle = Circle(color=RED, fill_opacity=0.5)
        self.play(Transform(square, circle))
        self.wait(1)

        self.play(FadeOut(square))
