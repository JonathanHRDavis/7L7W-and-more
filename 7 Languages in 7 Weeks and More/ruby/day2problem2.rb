class Tree
  attr_accessor :children, :node_name
  
  #def initialize(name, children=[])
    #@children = children
    #@node_name = name
  def initialize(tree = {})
    @node_name= tree.keys[0]
	@children = tree[@node_name].collect{|key, value| Tree.new({key => value})}
  end
  
  def visit_all(&block)
    visit &block
    children.each {|c| c.visit_all &block}
  end
  
  def visit(&block)
    block.call self
  end
end

ruby_tree = Tree.new({'grandpa' => { 'dad' => {'child 1' => {}, 'child 2' => {} }, 'uncle' => {'child 3' => {}, 'child 4' => {} } } } )

puts "visiting entire tree"
ruby_tree.visit_all {|node| puts node.node_name}
