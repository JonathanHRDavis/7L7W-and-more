// Jonathan Davis
// CSC 4010 - 800 - Programming Languages (Honors)
// Rust Version of CSC 3410's Program #1: Plane-Line Intersection 
// Dr. Martha J. Kosa


// "use" is Rust's version of import/include (Java/C++)
// Standard Input and Output
use std::io;

// Opening a File
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

//Error Handling
use std::error::Error;

//Command line arguments
use std::env;



#[derive(Copy, Clone)]
struct Point
{
    x: f32,
    y: f32,
	z: f32,
}


#[derive(Copy, Clone)]
struct Plane
{
	p1: Point,
	p2: Point,
	p3: Point,
}


#[derive(Copy, Clone)]
struct Line
{
	p1: Point,
	p2: Point,
}


fn point_point_subtraction(p1: Point, p2: Point) -> Point
{
	let x = p1.x - p2.x;
	let y = p1.y - p2.y;
	let z = p1.z - p2.z;
	return Point {x: x, y: y, z: z};
}


fn cross_product(p1: Point, p2: Point) -> Point
{
	//a x b (a cross b) = 
	//a_y*b_z - a_z*b_y (the first component of a x b)
	//a_z*b_x - a_x*b_z (the second component of a x b)
	//a_x*b_y - a_y*b_x (the third component of a x b)
	
	let x = (p1.y * p2.z) - (p1.z * p2.y);
	let y = (p1.z * p2.x) - (p1.x * p2.z);
	let z = (p1.x * p2.y) - (p1.y * p2.x);
	
	return Point {x: x, y: y, z: z};
}


fn dot_product(p1: Point, p2: Point) -> f32
{

	//a.b (a dot b) = a_x*b_x + a_y*b_y + a_z*b_z
	return (p1.x * p2.x) + (p1.y * p2.y) + (p1.z * p2.z)
	
}


fn normal(plane: Plane) -> Point
{
	//n = (PB - PA) x (PC - PA)
	let pbpa: Point = point_point_subtraction(plane.p2, plane.p1);
	let pcpa: Point = point_point_subtraction(plane.p3, plane.p1);
	return cross_product(pbpa, pcpa);
	
}


fn multiply_point(scalar: f32, point: Point) -> Point
{
	let x = scalar * point.x;
	let y = scalar * point.y;
	let z = scalar * point.z;
	
	return Point {x: x, y: y, z: z};
}


fn add_point(point1: Point, point2: Point) -> Point
{
	let x = point1.x + point2.x;
	let y = point1.y + point2.y;
	let z = point1.z + point2.z;
	
	return Point {x: x, y: y, z: z};
}


fn parametric(plane: Plane, line: Line) -> Point
{
	//α = n.(P0-P1)/n.(P2-P1)
	
	let n = normal(plane);
	let denom = dot_product(n, point_point_subtraction(line.p2, line.p1));
	let num = dot_product(n, point_point_subtraction(plane.p1, line.p1));
	let alpha = num / denom;
	
	//P(α) = (1-α)P1 + α*P2
	
	let addend1 = multiply_point((1.0 - alpha), line.p1);
	let addend2 = multiply_point(alpha, line.p2);
	let parametric = add_point(addend1, addend2);
	
	return parametric;
}


fn make_point(p1: &str, p2: &str, p3: &str) -> Point
{
	let x = p1.parse::<f32>().unwrap();
	let y = p2.parse::<f32>().unwrap();
	let z = p3.parse::<f32>().unwrap();
	
	return Point {x:x, y:y, z:z};
}


macro_rules! print_point
{
	($point: ident) =>
	(
		print!("\n({}, {}, {})\n", $point.x, $point.y, $point.z);
	)
}


macro_rules! read_float 
{
	($message: ident, $float_name: ident, $stdin: ident, $buffer: ident) =>
	(
		print!("{}", $message);
		io::stdout().flush().unwrap();
		
		match $stdin.read_line(&mut $buffer)
		{
			//_n is the nuber of bytes read
			Ok(_n) =>
			{
				$buffer.pop();
				$buffer.pop();
				
				$float_name = $buffer.as_str().parse::<f32>().unwrap();
				
				println!("{}", $float_name);
				$buffer.clear();
			}
			Err(error) => println!("Error: {}", error),
		}
		
	)
}


fn main() 
{
	let args: Vec<_> = env::args().collect();
	
	//The values will be entered from a file. (Not the way the original 3410 assignment required)
    if args.len() > 1 
	{
	
	//Create a path to the desired file
    let path = Path::new(&args[1]);
    let display = path.display();
	
    let mut file = match File::open(&path) 
	{
        Err(why) => panic!("Could not open file {}: {}", display, why.description()),
        Ok(file) => file,
    };

    let mut s = String::new();
	
    match file.read_to_string(&mut s) 
	{
        Err(why) => panic!("Could not read file {}: {}", display, why.description()),
        Ok(_) => 
		{
			let string_vector: Vec<&str> = s.split_whitespace().collect();
			
			let point1: Point = make_point(string_vector[0], string_vector[1], string_vector[2]);
			let point2: Point = make_point(string_vector[3], string_vector[4], string_vector[5]);
			let point3: Point = make_point(string_vector[6], string_vector[7], string_vector[8]);
			let point4: Point = make_point(string_vector[9], string_vector[10], string_vector[11]);
			let point5: Point = make_point(string_vector[12], string_vector[13], string_vector[14]);
			
			let plane: Plane = Plane {p1: point1, p2: point2, p3: point3};
			let line: Line = Line {p1: point4, p2: point5};
		
			let result = parametric(plane, line);

			print!("\nThe plane-line intersection is: ");
			println!("({}, {}, {})", result.x, result.y, result.z);
		}
    }
	
    }
	// The values will be entered from standard input. (More closely resembles the original 3410 assignment)
	else
	{
		let message1 = "Enter the x-coordinate of the point on the plane: "; 
		let message2 = "Enter the y-coordinate of the point on the plane: "; 
		let message3 = "Enter the z-coordinate of the point on the plane: "; 
		let message4 = "Enter the x-coordinate of the point on the line: "; 
		let message5 = "Enter the y-coordinate of the point on the line: "; 
		let message6 = "Enter the z-coordinate of the point on the line: "; 
	
		let mut buffer = String::new();
		let stdin: std::io::Stdin = io::stdin();
		
		let mut x: f32 = 0.0;
		let mut y: f32 = 0.0;
		let mut z: f32 = 0.0;
		
		read_float!(message1, x, stdin, buffer);
		read_float!(message2, y, stdin, buffer);
		read_float!(message3, z, stdin, buffer);
		let point1: Point = Point {x, y, z};
		print_point!(point1);
		
		read_float!(message1, x, stdin, buffer);
		read_float!(message2, y, stdin, buffer);
		read_float!(message3, z, stdin, buffer);
		let point2: Point = Point {x, y, z};
		print_point!(point2);
		
		read_float!(message1, x, stdin, buffer);
		read_float!(message2, y, stdin, buffer);
		read_float!(message3, z, stdin, buffer);
		let point3: Point = Point {x, y, z};
		print_point!(point3);
		
		read_float!(message4, x, stdin, buffer);
		read_float!(message5, y, stdin, buffer);
		read_float!(message6, z, stdin, buffer);
		let point4: Point = Point {x, y, z};
		print_point!(point4);
		
		read_float!(message4, x, stdin, buffer);
		read_float!(message5, y, stdin, buffer);
		read_float!(message6, z, stdin, buffer);
		let point5: Point = Point {x, y, z};
		print_point!(point5);
		
		let plane = Plane {p1: point1, p2: point2, p3: point3};
		let line = Line {p1: point4, p2: point5};
		
		let result = parametric(plane, line);

		println!("\n\n({}, {}, {})", result.x, result.y, result.z);		
	}
}