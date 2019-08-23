package main
import (
    "fmt"
    "strconv"
    "strings"
)

type Point struct {
    X float32
    Y float32
    Z float32
}

type Plane struct {
    p1 Point
    p2 Point
    p3 Point
}

type Line struct {
    p1 Point
    p2 Point
}

func point_point_subtraction(p1 Point, p2 Point) Point {
    x := p1.X - p2.X
    y := p1.Y - p2.Y
    z := p1.Z - p2.Z
    return Point {X: x,Y: y,Z: z}
}

func cross_product(p1 Point, p2 Point) Point {
    x := (p1.Y * p2.Z) - (p1.Z * p2.Y)
    y := (p1.Z * p2.X) - (p1.X * p2.Z)
    z := (p1.X * p2.Y) - (p1.Y * p2.X)

    return Point {X: x, Y: y, Z: z}
}

func dot_product(p1 Point, p2 Point) float32 {
    return (p1.X * p2.X) + (p1.Y * p2.Y) + (p1.Z * p2.Z)
}

func normal(plane Plane) Point {
    pbpa := point_point_subtraction(plane.p2, plane.p1)
    pcpa := point_point_subtraction(plane.p3, plane.p1)
    return cross_product(pbpa, pcpa)
}

func multiply_point(scalar float32, point Point) Point {
    x := scalar * point.X
    y := scalar * point.Y
    z := scalar * point.Z

    return Point {X: x, Y: y, Z: z}
}

func add_point(point1 Point, point2 Point) Point {
    x := point1.X + point2.X
    y := point1.Y + point2.Y
    z := point1.Z + point2.Z

    return Point {X: x, Y: y, Z: z}
}

func parametric(plane Plane, line Line) Point {
    n := normal(plane)
    denom :=  dot_product(n, point_point_subtraction(line.p2, line.p1))
    num := dot_product(n, point_point_subtraction(plane.p1, line.p1))
    alpha :=  num / denom

    //check if needed to be 1.0
    addend1 := multiply_point((1 - alpha), line.p1)
    addend2 := multiply_point(alpha, line.p2)
    parametric := add_point(addend1, addend2)

    return parametric
}

func make_point(p1 string, p2 string, p3 string) Point {
    x, _ := strconv.ParseFloat(strings.TrimSpace(p1), 32)
    y, _ := strconv.ParseFloat(strings.TrimSpace(p2), 32)
    z, _ := strconv.ParseFloat(strings.TrimSpace(p3), 32)

    return Point{X: float32(x), Y: float32(y), Z: float32(z)}
}

func print_point(point Point) {

}

func get_point_from_user(msg string) string {
    var input string
    fmt.Print(msg)
    _, err := fmt.Scanln(&input)
    if err != nil  {
        fmt.Println("Error: ", err)
    }
    return input
}


func main() {
    message1 := "Enter the x-coordinate of the point on the plane: "
    message2 := "Enter the y-coordinate of the point on the plane: "
    message3 := "Enter the z-coordinate of the point on the plane: "
    message4 := "Enter the x-coordinate of the point on the line: "
    message5 := "Enter the y-coordinate of the point on the line: "
    message6 := "Enter the z-coordinate of the point on the line: "


    var p1 string
    var p2 string
    var p3 string


    p1 = get_point_from_user(message1)
    p2 = get_point_from_user(message2)
    p3 = get_point_from_user(message3)
    plane_p1 := make_point(p1, p2, p3)
    fmt.Printf("(%s, %s, %s)\n", p1, p2, p3)

    p1 = get_point_from_user(message1)
    p2 = get_point_from_user(message2)
    p3 = get_point_from_user(message3)
    plane_p2 := make_point(p1, p2, p3)
    fmt.Printf("(%s, %s, %s)\n", p1, p2, p3)

    p1 = get_point_from_user(message1)
    p2 = get_point_from_user(message2)
    p3 = get_point_from_user(message3)
    plane_p3 := make_point(p1, p2, p3)
    fmt.Printf("(%s, %s, %s)\n", p1, p2, p3)

    p1 = get_point_from_user(message4)
    p2 = get_point_from_user(message5)
    p3 = get_point_from_user(message6)
    line_p1 := make_point(p1, p2, p3)
    fmt.Printf("(%s, %s, %s)\n", p1, p2, p3)

    p1 = get_point_from_user(message4)
    p2 = get_point_from_user(message5)
    p3 = get_point_from_user(message6)
    line_p2 := make_point(p1, p2, p3)
    fmt.Printf("(%s, %s, %s)\n", p1, p2, p3)

    plane := Plane {p1: plane_p1, p2: plane_p2, p3: plane_p3}
    line := Line {p1: line_p1, p2: line_p2}

    result := parametric(plane, line)

    fmt.Println("\n\n",result.X, result.Y, result.Z)
}
