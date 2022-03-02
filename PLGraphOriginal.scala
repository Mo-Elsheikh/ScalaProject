import junit.runner.Version.id
import org.graphstream.graph.{Edge, Graph, Node}
import org.graphstream.graph.implementations.{MultiGraph, SingleGraph, SingleNode}

import java.{lang, util}
import scala.io.Source

/*
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Student: Mohamed Elsheikh
 * Description: Homework 01 - PLGraph
 */



object PLGraphOriginal {

  val PL_CSV_FILE = "pl.csv"
  val USER_DIR    = System.getProperty("user.dir")
  val STYLE       = "stylesheet.css"

  def main(args: Array[String]): Unit = {

    // create the graph
    val graph = new MultiGraph("PL")
    graph.addAttribute("ui.stylesheet", "url('file://" + USER_DIR + "/" + STYLE + "')")
    graph.addAttribute("ui.antialias")

    // TODO: parse the PL_CSV_FILE to create a directed graph of PLs

    // create nodes Prolog,ISO Prolog
    var node: Node = graph.addNode("Prolog")
    node.addAttribute("ui.label", "Prolog")
    node = graph.addNode("ISO Prolog")
    node.addAttribute("ui.label", "ISO Prolog")
    // create edges Prolog,ISO Prolog
    var edge: Edge = graph.addEdge("Prolog ISO Prolog", "Prolog", "ISO Prolog", true)

    // create nodes BASIC,VB,VB.NET
    var node1: Node = graph.addNode("BASIC")
    node1.addAttribute("ui.label", "BASIC")
    node1 = graph.addNode("VB")
    node1.addAttribute("ui.label", "VB")
    node1 = graph.addNode("VB.NET")
    node1.addAttribute("ui.label", "VB.NET")
    // create edges BASIC,VB,VB.NET
    var edge1: Edge = graph.addEdge("BASIC VB", "BASIC", "VB", true)
    edge1 = graph.addEdge("VB VB.NET", "VB", "VB.NET", true)

    // create nodes COBOL,COBOL02
    var node2: Node = graph.addNode("COBOL")
    node2.addAttribute("ui.label", "COBOL")
    node2 = graph.addNode("COBOL02")
    node2.addAttribute("ui.label", "COBOL02")
    // create edges Prolog,ISO Prolog
    var edge2: Edge = graph.addEdge("COBOL COBOL02", "COBOL", "COBOL02", true)


    // create nodes Fortran,Fortran04
    var node3: Node = graph.addNode("Fortran")
    node3.addAttribute("ui.label", "Fortran")
    node3 = graph.addNode("Fortran04")
    node3.addAttribute("ui.label", "Fortran04")
    // create edges Prolog,ISO Prolog
    var edge3: Edge = graph.addEdge("Fortran Fortran04", "Fortran", "Fortran04", true)


    // create nodes Algol,Pascal,Ada,Spark
    var node4: Node = graph.addNode("Algol")
    node4.addAttribute("ui.label", "Algol")
    node4 = graph.addNode("Pascal")
    node4.addAttribute("ui.label", "Pascal")
    node4 = graph.addNode("Ada")
    node4.addAttribute("ui.label", "Ada")
    node4 = graph.addNode("Spark")
    node4.addAttribute("ui.label", "Spark")
    // create edges Algol,Pascal,Ada,Spark
    var edge4: Edge = graph.addEdge("Algol Pascal", "Algol", "Pascal", true)
    edge4 = graph.addEdge("Pascal Ada", "Pascal", "Ada", true)
    edge4 = graph.addEdge("Ada Spark", "Ada", "Spark", true)


    //create nodes Lisp,Haskell,Scala ,Simula,Smalltalk ,Java ,Scala ,Java,Java14
    //C C++,C# C,Perl Perl,Python Python,Python2 python2,Python3 ,Perl,PHP ,
    // Perl,Javascript ,Perl,Ruby
    var node5: Node = graph.addNode("Simula")
    node5.addAttribute("ui.label", "Simula")
    node5 = graph.addNode("Smalltalk")
    node5.addAttribute("ui.label", "Smalltalk")
    node5 = graph.addNode("Java")
    node5.addAttribute("ui.label", "Java")
    node5 = graph.addNode("Scala")
    node5.addAttribute("ui.label", "Scala")
    node5 = graph.addNode("Java14")
    node5.addAttribute("ui.label", "Java14")
    node5 = graph.addNode("Haskell")
    node5.addAttribute("ui.label", "Haskell")
    node5 = graph.addNode("Lisp")
    node5.addAttribute("ui.label", "Lisp")
    node5 = graph.addNode("C++")
    node5.addAttribute("ui.label", "C++")
    node5 = graph.addNode("C#")
    node5.addAttribute("ui.label", "C#")
    node5 = graph.addNode("C")
    node5.addAttribute("ui.label", "C")
    node5 = graph.addNode("Perl")
    node5.addAttribute("ui.label", "Perl")
    node5 = graph.addNode("Ruby")
    node5.addAttribute("ui.label", "Ruby")
    node5 = graph.addNode("PHP")
    node5.addAttribute("ui.label", "PHP")
    node5 = graph.addNode("Javascript")
    node5.addAttribute("ui.label", "Javascript")
    node5 = graph.addNode("Python")
    node5.addAttribute("ui.label", "Python")
    node5 = graph.addNode("Python2")
    node5.addAttribute("ui.label", "Python2")
    node5 = graph.addNode("Python3")
    node5.addAttribute("ui.label", "Python3")
    // create edges Lisp,Haskell,Scala ,Simula,Smalltalk ,Java ,Scala ,Java,Java14
    //C C++,C# C,Perl Perl,Python Python,Python2 python2,Python3 ,Perl,PHP ,
    // Perl,Javascript ,Perl,Ruby
    var edge5: Edge = graph.addEdge("Simula Smalltalk", "Simula", "Smalltalk", true)
    edge5 = graph.addEdge("Smalltalk  Java", "Smalltalk", "Java", true)
    edge5 = graph.addEdge("Java Scala", "Java", "Scala", true)
    edge5 = graph.addEdge("Java Java14", "Java", "Java14", true)
    edge5 = graph.addEdge("Lisp Haskell", "Lisp", "Haskell", true)
    edge5 = graph.addEdge("Haskell Scala", "Haskell", "Scala", true)
    edge5 = graph.addEdge("C++ Java", "C++", "Java", true)
    edge5 = graph.addEdge("C++ C#", "C++", "C#", true)
    edge5 = graph.addEdge("C C++", "C", "C++", true)
    edge5 = graph.addEdge("C Perl", "C", "Perl", true)
    edge5 = graph.addEdge("Perl Ruby", "Perl", "Ruby", true)
    edge5 = graph.addEdge("Perl javascript", "Perl", "Javascript", true)
    edge5 = graph.addEdge("Perl PHP", "Perl", "PHP", true)
    edge5 = graph.addEdge("Perl Python", "Perl", "Python", true)
    edge5 = graph.addEdge("Python Python2", "Python", "Python2", true)
    edge5 = graph.addEdge("Python2 Python3", "Python2", "Python3", true)

   graph.display()
  }

}
