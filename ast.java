import java.io.*;
import java.util.*;

// **********************************************************************
// The ASTnode class defines the nodes of the abstract-syntax tree that
// represents a Little program.
//
// Internal nodes of the tree contain pointers to children, organized
// either in a list (for nodes that may have a variable number of children)
// or as a fixed set of fields.
//
// The nodes for literals and ids contain line and character number
// information; for string literals and identifiers, they also contain a
// string; for integer literals, they also contain an integer value.
//
// Here are all the different kinds of AST nodes and what kinds of children
// they have.  All of these kinds of AST nodes are subclasses of "ASTnode".
// Indentation indicates further subclassing:
//
//     Subclass            Kids
//     --------            ----
//     ProgramNode         DeclListNode
//     DeclListNode        list of DeclNode
//     DeclNode:
//       VarDeclNode       TypeNode, IdNode, int
//       FnDeclNode        TypeNode, IdNode, FormalsListNode, FnBodyNode
//       FormalDeclNode    TypeNode, IdNode
//     FormalsListNode     list of FormalDeclNode
//     FnBodyNode          DeclListNode, StmtListNode
//     StmtListNode        list of StmtNode
//     ExpListNode         list of ExpNode
//
//     TypeNode:
//       IntNode           -- none --
//       DblNode           -- none --
//       VoidNode          -- none --
//
//     StmtNode:
//    *  AssignStmtNode      AssignNode
//       PreIncStmtNode      IdNode
//       PreDecStmtNode      IdNode
//       PostIncStmtNode     IdNode
//       PostDecStmtNode     IdNode
//       ReadIntStmtNode     IdNode
//       ReadDblStmtNode     IdNode
//    *  WriteIntStmtNode    ExpNode
//       WriteDblStmtNode    ExpNode
//       WriteStrStmtNode    ExpNode
//    *  IfStmtNode          ExpNode, DeclListNode, StmtListNode
//    *  IfElseStmtNode      ExpNode, DeclListNode, StmtListNode,
//                                    DeclListNode, StmtListNode
//    *  WhileStmtNode       ExpNode, DeclListNode, StmtListNode
//    *  CallStmtNode        CallExpNode
//    *  ReturnStmtNode      ExpNode
//
//    NOTE:  * indicates possibility of a post-inc/dec expression
//
//     ExpNode:
//       IntLitNode          -- none --
//       DblLitNode          -- none --
//       StringLitNode       -- none --
//       IdNode              -- none --
//       CallExpNode         IdNode, ExpListNode
//       UnaryExpNode        
//         UnaryMinusNode    ExpNode
//         NotNode           ExpNode
//         PlusPlusNode      IdNode
//         MinusMinusNode    IdNode
//       BinaryExpNode       ExpNode, ExpNode
//         AssignNode
//         ArithBinExpNode
//           PlusNode     
//           MinusNode
//           TimesNode
//           DivideNode
//         EqualityBinExpNode
//           EqualsNode
//           NotEqualsNode
//           LessNode
//           GreaterNode
//           LessEqNode
//           GreaterEqNode
//         LogicalBinExpNode
//           AndNode
//           OrNode
//
// Here are the different kinds of AST nodes again, organized according to
// whether they are leaves, internal nodes with lists of kids, or internal
// nodes with a fixed number of kids:
//
// (1) Leaf nodes:
//        IntNode,   DblNode,  VoidNode,  IntLitNode,  DblLitNode, StringLitNode,
//        IdNode
//
// (2) Internal nodes with (possibly empty) lists of children:
//        DeclListNode, FormalsListNode, StmtListNode, ExpListNode
//
// (3) Internal nodes with fixed numbers of kids:
//       ProgramNode,      VarDeclNode,     FnDeclNode,       FormalDeclNode,
//       FnBodyNode,       TypeNode,        AssignStmtNode,
//       PreIncStmtNode,   PreDecStmtNode,  PostIncStmtNode,  PostDecStmtNode,
//       ReadIntStmtNode,  ReadDblStmtNode, WriteIntStmtNode, WriteDblStmtNode
//       WriteStrStmtNode  IfStmtNode,      IfElseStmtNode,   WhileStmtNode,
//       CallStmtNode,     ReturnStmtNode,  CallExpNode,
//       UnaryExpNode,     UnaryMinusNode,  NotNode,          PlusPlusNode,
//       MinusMinusNode    BinaryExpNode    AssignNode,
//       ArithmeticBinExpNode               PlusNode,         MinusNode,
//       TimesNode,        DivideNode,      EqualityBinExpNode,
//       EqualsNode,       NotEqualsNode,
//       LessNode,         GreaterNode,     LessEqNode,       GreaterEqNode,
//       LogicalBinExpNode,                 AndNode,          OrNode
//
// **********************************************************************

// **********************************************************************
// ASTnode class (base class for all other kinds of nodes)
// **********************************************************************
abstract class ASTnode { 
    protected static final boolean DEBUG = false;
    protected static final String INT_TYPE = "int";
    protected static final String DBL_TYPE = "double";
    protected static final String ERR_TYPE = "ERR";
    protected static final String VOID_TYPE = "void";

    // registers
    public static final String FP = "$fp";
    public static final String SP = "$sp";
    public static final String RA = "$ra";
    public static final String V0 = "$v0";
    public static final String V1 = "$v1";
    public static final String A0 = "$a0";
    public static final String T0 = "$t0";
    public static final String T1 = "$t1";
    public static final String T2 = "$t2";
    public static final String T3 = "$t3";
    public static final String T4 = "$t4";
    public static final String T5 = "$t5";
    public static final String T6 = "$t6";
    public static final String F0 = "$f0";
    public static final String F2 = "$f2";
    public static final String F12 = "$f12";


    /********************************************************************
     *
     *    CODEGEN DATA STRUCTURES AND METHODS
     *
     *******************************************************************/
    // List of post-inc/dec nodes to be handled after statements */
    protected static List<PostIncStmtNode> postIncList = new ArrayList<PostIncStmtNode>();
    protected static List<PostDecStmtNode> postDecList = new ArrayList<PostDecStmtNode>();

    /** Called at the end of each statement with post-inc/dec expression **/
    protected void doPostGen() {
	// Generate post code
	Iterator it = postIncList.iterator();
	while (it.hasNext()) {
	    ((PostIncStmtNode)it.next()).codeGen();
	}
	it = postDecList.iterator();
	while (it.hasNext()) {
	    ((PostDecStmtNode)it.next()).codeGen();
	}
	// Clear the lists
	postIncList.clear();
	postDecList.clear();
    }

    // mark whether a main function is provided
    protected static boolean HAS_MAIN = false;

    // HashMap for label storage (StrLits)
    HashMap<String, String> labelMap = new HashMap<String, String>();

    // Store the current function return label
    protected static String FN_EXIT;

    // Store the return value of current function being evaluated
    protected static String FN_RETURN;

    // keep track of offset from framepointer as locals are added
    protected static int offset;

    // Method to convert ints to doubles
    protected static void intToDouble() {
	/**************************************************************
	 *  Pops an int already on the stack, and pushes an equivalent
	 *  double back onto the stack 
	 *
	 *  T0 holds MSB, T1 holds LSB, T2 holds the int          
	 *  T3 counts the exponent, T4 holds temporary calculations
	 *  T5 is set to 1 for finding the exponenet
	 *  T6 holds the biased exponenet
	 **************************************************************/
	// Labels
	String zero_lab = Codegen.nextLabel();
	String do_fraction = Codegen.nextLabel();
	String do_exponent = Codegen.nextLabel();
	String exit_lab = Codegen.nextLabel();
	Codegen.genComment("INT->DBL");

	// Pop the int
	Codegen.genPop(T2, 4);
	// Check for zero
	Codegen.generate("beqz", T2, zero_lab);
	// Get the sign bit
	Codegen.generate("and", T0, T2, "0x80000000");

	/** Get the exponent **/
	Codegen.generate("move", T4, T2);      // copy int value
	Codegen.generate("and", T3, T3, "$0"); // 0 counter
	Codegen.generate("li", T5, "1");       // used to calculate exponent

	// Shift right until int == 1.  Count for exponent
	Codegen.genLabel(do_exponent);
	Codegen.generate("beq", T4, T5, do_fraction); // is 1?
	Codegen.generate("srl", T4, T4, "1");         // shift right
	Codegen.generate("add", T3, T3, "1");         // increment counter
	Codegen.generate("j", do_exponent);           // repeat
	
	// Adjust exponent for double precision
	Codegen.genLabel(do_fraction);
	Codegen.generate("add", T6, T3, "1023");      // add bias
	// exponent sign always positive (i.e. zero).  So just shift 20.
	Codegen.generate("sll", T6, T6, "20");
	// Append to MSB
	Codegen.generate("or", T0, T0, T6);

	/** Get MSB part of fraction **/
	Codegen.generate("li", T4, "32");      // 
	Codegen.generate("sub", T3, T4, T3);   // amount needed to shift off top bit
	Codegen.generate("move", T4, T2);      // Get another copy of int
	Codegen.generate("sll", T4, T4, T3);   // shift off top bit
	Codegen.generate("srl", T4, T4, "12"); // shift into position
	Codegen.generate("or", T0, T0, T4);    // OR to complete MSB

	/** Get LSB part of fraction **/
	Codegen.generate("sll", T1, T4, "20"); // top 20 bits are for the MSB
	Codegen.generate("j", exit_lab);       // ready to exit

	/** Case where F0 is 0 **/
	Codegen.genLabel(zero_lab);
	Codegen.generate("and", T0, T0, "$0"); // 0 MSB and LSB
	Codegen.generate("and", T1, T1, "$0"); 

	/** Exit **/
	Codegen.genLabel(exit_lab);
	Codegen.genPush(T0, 4); // Push MSB
	Codegen.genPush(T1, 4); // Push LSB
	
    }

    // Static list of Formals.  Used to convert ints passed as doubles
    protected static List<String> paramTypes;

    /*******************************************************************
     *                     End CodeGen
    *******************************************************************/

    

    // every subclass must provide an unparse operation
    abstract public void unparse(PrintWriter p, int indent);

    // this method can be used by the unparse methods to do indenting
    protected void doIndent(PrintWriter p, int indent) {
	for (int k=0; k<indent; k++) p.print(" ");
    }

    // methods for type checking
    protected static boolean isErrType(String T) {
	return T.equals(ERR_TYPE);
    }

    protected static boolean isVoidType(String T) {
	return T.equals(VOID_TYPE);
    }

    protected static boolean isFnType(String T) {
	return T.indexOf("->") != -1;
    }

    protected static boolean isIntType(String T) {
	return T.equals(INT_TYPE);
    }

    protected static boolean isDblType(String T) {
	return T.equals(DBL_TYPE);
    }

    protected static boolean isNumericType(String T) {
	return T.equals(INT_TYPE) || T.equals(DBL_TYPE);
    }

    protected static boolean compatibleTypes(String T1, String T2) {
	// T1 is a formal's type, T2 is an actual's type
	// OK iff same type or dbl, int
	return T1.equals(T2) || (isDblType(T1) && isIntType(T2));
    }
}

// **********************************************************************
// ProgramNode,  DeclListNode, FormalsListNode, FnBodyNode,
// StmtListNode, ExpListNode
// **********************************************************************
class ProgramNode extends ASTnode {
    public ProgramNode(DeclListNode L) {
	myDeclList = L;
    }

    /** processNames
     *
     * create an empty symbol table for the outermost scope, then
     * process all of the globals and functions in the program
     **/
    public void processNames() {
	SymTab S = new SymTab();
	myDeclList.processNames(S);

	// Check whether a main function was provided
	if (!HAS_MAIN)
	    Errors.fatal(0,0,"No main function");
    }

    /** typeCheck **/
    public void typeCheck() {
	myDeclList.typeCheck();
    }

    /** codeGen **/
    public void codeGen() {
	myDeclList.codeGen();
    }

    public void unparse(PrintWriter p, int indent) {
	myDeclList.unparse(p, indent);
    }

    // 1 kid
    private DeclListNode myDeclList;
}

class DeclListNode extends ASTnode {
    public DeclListNode(List<DeclNode> L) {
	myDecls = L;
    }

    /** processNames
     *
     * given: a symbol table S
     * do:    process all of the decls in the list
     **/
    public void processNames(SymTab S) {
	Iterator it = myDecls.iterator();
	try {
	    while (it.hasNext()) {
		((DeclNode)it.next()).processNames(S);
	    }
	} catch (NoSuchElementException ex) {
	    System.err.println("unexpected NoSuchElementException in DeclListNode.processNames");
	    System.exit(-1);
	}
	
    }

    /** typeCheck **/
    public void typeCheck() {
	Iterator it = myDecls.iterator();
	try {
	    while (it.hasNext()) {
		((DeclNode)it.next()).typeCheck();
	    }
	} catch (NoSuchElementException ex) {
	    System.err.println("unexpected NoSuchElementException in DeclListNode.typeCheck");
	    System.exit(-1);
	}
	
    }

    /** codeGen **/
    public void codeGen() {
	if (myDecls.size() != 0) {  // no codegen if there are no declarations
	    for (DeclNode decl: myDecls) {
		decl.codeGen();
	    }
	}
    }
    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	Iterator it = myDecls.iterator();
	try {
	    while (it.hasNext()) {
		((DeclNode)it.next()).unparse(p, indent);
	    }
	} catch (NoSuchElementException ex) {
	    System.err.println("unexpected NoSuchElementException in DeclListNode.unparse");
	    System.exit(-1);
	}
    }

    // list of kids (DeclNodes)
    private List<DeclNode> myDecls;
}

class FormalsListNode extends ASTnode {
    public FormalsListNode(List<FormalDeclNode> L) {
	myFormals = L;
    }

    /** processNames
     *
     * given: a symbol table S
     * do:    process all of the formals in the list
     **/
    public LinkedList processNames(SymTab S) {
	LinkedList L = new LinkedList();
	Iterator it = myFormals.iterator();
	try {
	    while (it.hasNext()) {
		Sym sym = ((FormalDeclNode)it.next()).processNames(S);
		if (sym != null) L.add(sym);
	    }
	} catch (NoSuchElementException ex) {
	    System.err.println("unexpected NoSuchElementException in FormalsListNode.processNames");
	    System.exit(-1);
	}
	return L;
    }

    /** length **/
    public int length() {
	return myFormals.size();
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	Iterator it = myFormals.iterator();
	try {
	    while (it.hasNext()) {
		((FormalDeclNode)it.next()).unparse(p, indent);
		if (it.hasNext()) {
		    p.print(", ");
		}
	    }
	} catch (NoSuchElementException ex) {
	    System.err.println("unexpected NoSuchElementException in FormalsListNode.unparse");
	    System.exit(-1);
	}
    }

    /** paramSize
     *
     *  Used by FnDeclNode codeGen method to calculate 
     *  local param offset
     **/

    // list of kids (FormalDeclNodes)
    private List<FormalDeclNode> myFormals;
}

class FnBodyNode extends ASTnode {
    public FnBodyNode(DeclListNode declList, StmtListNode stmtList) {
	myDeclList = declList;
	myStmtList = stmtList;
    }

    /** processNames **/
    public void processNames(SymTab S) {
	myDeclList.processNames(S);
	myStmtList.processNames(S);
    }

    /** typeCheck **/
    public void typeCheck(String returnType) {
	myStmtList.typeCheck(returnType);
    }

    /** codeGen **/
    public void codeGen() {
	// Local variable space already allocated
	// Generate code for statements 
	myStmtList.codeGen();
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	if (myDeclList != null) myDeclList.unparse(p, indent+2);
	if (myStmtList != null) myStmtList.unparse(p, indent+2);
    }

    // 2 kids
    private DeclListNode myDeclList;
    private StmtListNode myStmtList;
}

class StmtListNode extends ASTnode {
    public StmtListNode(List<StmtNode> L) {
	myStmts = L;
    }

    /** processNames **/
    public void processNames(SymTab S) {
	Iterator it = myStmts.iterator();
	try {
	    while (it.hasNext()) {
		((StmtNode)it.next()).processNames(S);
	    }
	} catch (NoSuchElementException ex) {
	    System.err.println("unexpected NoSuchElementException in StmtListNode.processNames");
	    System.exit(-1);
	}
    }

    /** typeCheck **/
    public void typeCheck(String returnType) {
	Iterator it = myStmts.iterator();
	try {
	    while (it.hasNext()) {
		((StmtNode)it.next()).typeCheck(returnType);
	    }
	} catch (NoSuchElementException ex) {
	    System.err.println("unexpected NoSuchElementException in StmtListNode.processNames");
	    System.exit(-1);
	}
    }

    /** codeGen **/
    public void codeGen() {
	Iterator it = myStmts.iterator();
	try {
	    while (it.hasNext()) {
		((StmtNode)it.next()).codeGen();
	    }
	} catch (NoSuchElementException ex) {
	    System.err.println("unexpected NoSuchElementException in StmtListNode.processNames");
	    System.exit(-1);
	}
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	// indent for each stmt is done here
	// each stmt is expected to end with a newline
	Iterator it = myStmts.iterator();
	try {
	    while (it.hasNext()) {
		doIndent(p, indent);
		((StmtNode)it.next()).unparse(p, indent);
	    }
	} catch (NoSuchElementException ex) {
	    System.err.println("unexpected NoSuchElementException in StmtListNode.unparse");
	    System.exit(-1);
	}
    }

    // list of kids (StmtNodes)
    private List<StmtNode> myStmts;
}

class ExpListNode extends ASTnode {
    public ExpListNode(List<ExpNode> L) {
	myExps = L;
    }

    /** typeCheck **/
    public void typeCheck(LinkedList<String> L) {
	int k=0;
	Iterator it = myExps.iterator();
	try {
	    while (it.hasNext()) {
		ExpNode exp = (ExpNode)it.next();
		String actualT = exp.typeCheck();
		if (!isErrType(actualT)) {
		    String paramT = L.get(k);
		    if (!compatibleTypes(paramT, actualT)) {
			Errors.fatal(exp.linenum(), exp.charnum(),
				     "Type of actual does not match type of formal");
		    }
		}
		k++;
	    }
	} catch (NoSuchElementException ex) {
	    System.err.println("unexpected NoSuchElementException in ExpListNode.processNames");
	    System.exit(-1);
	}
    }

    /** codeGen 
     *
     *  Leaves each passed parameter on stack **/
    public void codeGen() {
	// This code evaluates each expression and leaves on stack
	Iterator it = myExps.iterator();
	Iterator it2 = paramTypes.iterator();
	try {
	    while (it.hasNext()) {
		ExpNode exp = (ExpNode)it.next();
		String paramType = (String)it2.next();
		// Push exp value onto stack
		exp.codeGen();
		// Match param against expected type
		if (exp.returnSize == 4 && isDblType(paramType)) {
		    /** If an int was passed, but function needs a double **/
		    intToDouble(); // convert
		}
	    }
	} catch (NoSuchElementException ex) {
	    System.err.println("unexpected NoSuchElementException in ExpListNode.processNames");
	    System.exit(-1);
	}
    }

    /** processNames **/
    public void processNames(SymTab S) {
	Iterator it = myExps.iterator();
	try {
	    while (it.hasNext()) {
		((ExpNode)it.next()).processNames(S);
	    }
	} catch (NoSuchElementException ex) {
	    System.err.println("unexpected NoSuchElementException in ExpListNode.processNames");
	    System.exit(-1);
	}
    }

    /** length **/
    public int length() {
	return myExps.size();
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	Iterator it = myExps.iterator();
	try {
	    while (it.hasNext()) {
		((ExpNode)it.next()).unparse(p, 0);
		if (it.hasNext()) {
		    p.print(", ");
		}
	    }
	} catch (NoSuchElementException ex) {
	    System.err.println("unexpected NoSuchElementException in ExpListNode.unparse");
	    System.exit(-1);
	}
    }

    // list of kids (ExpNodes)
    private List<ExpNode> myExps;
}

// **********************************************************************
// DeclNode and its subclasses
// **********************************************************************
abstract class DeclNode extends ASTnode {
    // note: only a formal decl needs to return a Sym
    //       but since we must declare the method here,
    //       we make all decl nodes return something
    //       (for non formal decls, the returned value
    //       is simply ignored)
    abstract public Sym processNames(SymTab S);
    abstract public void codeGen();

    // default version of typeCheck for var and formal decls
    public void typeCheck() {
    }
}

class VarDeclNode extends DeclNode {
    public VarDeclNode(TypeNode type, IdNode id) {
	myType = type;
	myId = id;
    }

    /** processNames
     *
     * given: a symbol table
     * do: if this name is declared void, error!
     *     if this name has already been declared in this scope, error!
     *     if no error, add name to local symbol table
     **/
    public Sym processNames(SymTab S) {
	String name = myId.name();
	boolean badDecl = false;
	if (isVoidType(myType.type())) {
	    Errors.fatal(myId.linenum(), myId.charnum(),
			 "Non-function declared void");
	    badDecl = true;
	}
	if (S.localLookup(name) != null) {
	    Errors.fatal(myId.linenum(), myId.charnum(),
			 "Multiply declared identifier");
	    badDecl = true;
	}
	if (! badDecl) {
	    try {
		Sym sym = new Sym(myType.type());
		
		/* Calculate sym object's offset */
		if (isIntType(myType.type())) {
		    sym.setOffset(offset); // no adjustment
		    offset += 4; // 4 for int type
		}
		else if (isDblType(myType.type())) {
		    sym.setOffset(offset + 4); // adjust for double size
		    offset += 8; // 8 for dbl type
		}		
		S.insert(name, sym);
		myId.link(sym);
	    } catch (DuplicateException ex) {
		System.err.println("unexpected DuplicateException in VarDeclNode.processNames");
		System.exit(-1);
	    } catch (EmptySymTabException ex) {
		System.err.println("unexpected EmptySymTabException in VarDeclNode.processNames");
		System.exit(-1);
	    }
	}
	return null;  // return value ignored
    }

    /** codeGen **/
    public void codeGen() {
	Sym sym = myId.sym();

	/* Check if local or global */
	if (sym.isGlobal()) {
	    // .data
	    Codegen.generate(".data");
	    // .align 2
	    Codegen.generate(".align 2");
	    // create global label
	    if (isDblType(sym.type()))
		Codegen.generateLabeled("_" + myId.name(), ".space 8", "");
	    else if (isIntType(sym.type()))
		Codegen.generateLabeled("_" + myId.name(), ".space 4", "");
	    Codegen.generate(".text");

	} else if (sym.isLocal()) {
	    // nothing to do here
	}
    }
	
    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	doIndent(p, indent);
	myType.unparse(p, 0);
	p.print(" ");
	myId.unparse(p, 0);
	p.println(";");
    }

    // 2 kids
    private TypeNode myType;
    private IdNode myId;
}

class FnDeclNode extends DeclNode {
    public FnDeclNode(TypeNode type,
		      IdNode id,
		      FormalsListNode formalList,
		      FnBodyNode body) {
	myType = type;
	myId = id;
	myFormalsList = formalList;
	myBody = body;
    }

    /** processNames
     *
     * given: a symbol table S
     * do:    If this name has already been declared in this scope
     *        then error
     *        else add name to local symbol table.
     *        In any case:
     *             enter new scope
     *             process formals
     *             if this fn not multiply decld
     *                update symtab entry with types of formals
     *             process body
     *             exit scope
     **/
    public Sym processNames(SymTab S) {
	String name = myId.name();
	FnSym sym = null;
	
	// Check for main function
	if (name.equals("main"))
	    HAS_MAIN = true;

	if (S.localLookup(name) != null) {
	    Errors.fatal(myId.linenum(), myId.charnum(),
			 "Multiply declared identifier");
	}
	else {
	    try {
		sym = new FnSym(myType.type(),
				myFormalsList.length());
		S.insert(name, sym);
		myId.link(sym);
	    } catch (DuplicateException ex) {
		System.err.println("unexpected DuplicateException in FnDeclNode.processNames");
		System.exit(-1);
	    } catch (EmptySymTabException ex) {
		System.err.println("unexpected EmptySymTabException in FnDeclNode.processNames");
		System.exit(-1);
	    }
	}
	S.addMap();
	
	/* Measure offset from FP for local variables */
	offset = 0;

	LinkedList L = myFormalsList.processNames(S);
	if (sym != null) sym.addFormals(L);

	/* Get size of parameters */
	paramSize = offset;   // this is set by myFormalsList.processNames()

	/* Increase offset for return address and control link */
	offset += 8; // 4 for each

	myBody.processNames(S);
	try {
	    S.removeMap();
	} catch (EmptySymTabException ex) {
		System.err.println("unexpected EmptySymTabException in FnDeclNode.processNames");
		System.exit(-1);
	}

	/* Get size of locals */
	localSize = offset - 8 - paramSize;

	return null;
    }

    /** typeCheck **/
    public void typeCheck() {
	myBody.typeCheck(myType.type());
    }

    /** codeGen **/
    public void codeGen() {
	// Text segment
	Codegen.generate(".text", "");

	// generate Exit label (for return statements)
	FN_EXIT = Codegen.nextLabel();
	// set global return value for conversion from int to double
	FN_RETURN = ((FnSym)myId.sym()).returnType();

	/** Fn Entry **/
	// create fn label
	if (myId.name().equals("main")) {
	    Codegen.generateLabeled(myId.name(), "", "");
	    Codegen.genComment("FN PREAMBLE");
	} else {
	    Codegen.generateLabeled("_" + myId.name(), "", "");
	    Codegen.genComment("FN PREAMBLE");
	}
	// push return address
	Codegen.genPush(RA, 4);
	// push control link
	Codegen.genPush(FP, 4);
	// set frame pointer
	Codegen.generateWithComment("addu", FP, SP, Integer.toString(paramSize+8), "Set FP");
	// allocate space for locals
	Codegen.generateWithComment("subu", SP, SP, Integer.toString(localSize), "Allocate space for locals");
	/** Fn Body **/
	// generate code for fn body
	Codegen.genComment("FN BODY");
	myBody.codeGen();

	/** Fn Exit **/
	Codegen.genComment("FN EXIT");
	// Generate exit label
	Codegen.genLabel(FN_EXIT);
	// load RA
	Codegen.generateIndexed("lw", RA, FP, -paramSize, "Load RA");
	// save control link
	Codegen.generateWithComment("move", T0, FP, "Save control link in case of int.");
	// restore fp
	Codegen.generateIndexed("lw", FP, FP, -(paramSize+4), "Restore FP");
	// restore sp
	Codegen.generateWithComment("move", SP, T0, "Restore SP");
	// return
	Codegen.generate("jr", RA);
	
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.println();
	doIndent(p, indent);
	myType.unparse(p, 0);
	p.print(" ");
	myId.unparse(p, 0);
	p.print("(");
	if (myFormalsList != null) myFormalsList.unparse(p, 0);
	p.println(") {");
	if (myBody != null) myBody.unparse(p, indent);
	doIndent(p, indent);
	p.println("}");
    }

    // size of params
    int paramSize;
    // size of locals
    int localSize;

    // 4 kids
    private TypeNode myType;
    private IdNode myId;
    private FormalsListNode myFormalsList;
    private FnBodyNode myBody;
}

class FormalDeclNode extends DeclNode {
    public FormalDeclNode(TypeNode type, IdNode id) {
	myType = type;
	myId = id;
    }

    /** processNames
     *
     * given: a symbol table S
     * do:    if this formal is declared void, error!
     *        else if this formal is multiply declared
     *        then give an error msg and return null
     *        else add a new entry to S and also return that Sym
     **/
    public Sym processNames(SymTab S) {
	String name = myId.name();
	boolean badDecl = false;
	Sym sym = null;
	if (isVoidType(myType.type())) {
	    Errors.fatal(myId.linenum(), myId.charnum(),
			 "Non-function declared void");
	    badDecl = true;
	}
	if (S.localLookup(name) != null) {
	    Errors.fatal(myId.linenum(), myId.charnum(),
			 "Multiply declared identifier");
	    badDecl = true;
	}
	if (! badDecl) {
	    try {
		sym = new Sym(myType.type());
		
		/* Calculate sym object's offset */
		if (isIntType(myType.type())) {
		    sym.setOffset(offset); // no adjustment
		    offset += 4; // 4 for int type
		}
		else if (isDblType(myType.type())) {
		    sym.setOffset(offset + 4); // adjust for double size
		    offset += 8; // 8 for dbl type
		}		

		S.insert(name, sym);
		myId.link(sym);
	    } catch (DuplicateException ex) {
		System.err.println("unexpected DuplicateException in FormalDeclNode.processNames");
		System.exit(-1);
	    } catch (EmptySymTabException ex) {
		System.err.println("unexpected EmptySymTabException in FormalDeclNode.processNames");
		System.exit(-1);
	    }
	}
	return sym;
    }

    /** codeGen **/
    public void codeGen() {
	// Nothing to do
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	doIndent(p, indent);
	myType.unparse(p, indent);
	p.print(" ");
	myId.unparse(p, indent);
    }

    // 2 kids
    private TypeNode myType;
    private IdNode myId;
}

// **********************************************************************
// TypeNode and its Subclasses
// **********************************************************************
abstract class TypeNode extends ASTnode {
    /* all subclasses must provide a type method */
    abstract public String type();
}

class IntNode extends TypeNode {
    public IntNode() {
    }

    /** type **/
    public String type() {
	return INT_TYPE;
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print(INT_TYPE);
    }
}

class DblNode extends TypeNode {
    public DblNode() {
    }

    /** type **/
    public String type() {
	return DBL_TYPE;
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print(DBL_TYPE);
    }
}

class VoidNode extends TypeNode {
    public VoidNode() {
    }

    public String type() {
	return VOID_TYPE;
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print(VOID_TYPE);
    }
}

// **********************************************************************
// StmtNode and its subclasses
// **********************************************************************

abstract class StmtNode extends ASTnode {
    abstract public void processNames(SymTab S);
    abstract public void typeCheck(String T);
    abstract public void codeGen();
}

class AssignStmtNode extends StmtNode {
    public AssignStmtNode(AssignNode e) {
	myExp = e;
    }

    /** processNames **/
    public void processNames(SymTab S) {
	myExp.processNames(S);
    }

    /** typeCheck **/
    public void typeCheck(String retType) {
	myExp.typeCheck();
    }

    /** codeGen **/
    public void codeGen() {
	Codegen.genComment("ASSN STMT");

	// evaluate assignment
	myExp.codeGen();

	// pop value off of stack
	if (myExp.returnSize() == 4)
	    Codegen.genPop(T0, 4);
	else if (myExp.returnSize() == 8)
	    Codegen.genPop(F0, 8);

	// check for post-inc/dec
	doPostGen();
    }

    public void unparse(PrintWriter p, int indent) {
	myExp.unparse(p,0,false);
	p.println(";");
    }

    // 1 kid
    private AssignNode myExp;
}

class PreIncStmtNode extends StmtNode {
    public PreIncStmtNode(IdNode id) {
	myId = id;
    }

    /** processNames **/
    public void processNames(SymTab S) {
	myId.processNames(S);
    }

    /** typeCheck **/
    public void typeCheck(String retType) {
	String T = myId.typeCheck();
	if (!isErrType(T)) {
	    if (!isIntType(T)) {
		Errors.fatal(myId.linenum(), myId.charnum(),
			     "Non-int identifier used with ++ or --");
	    }
	}
    }

    public void codeGen() {
	Codegen.genComment("PREINC");
	// push address of id onto stack
	myId.genAddr();

	// evaluate rhs
	myId.codeGen();
	
	// load value into register, increment, then store
	if (isIntType(myId.type())) {
	    Codegen.genPop(T0, 4); // get id val
	    Codegen.genPop(T1, 4); // get id address
	    Codegen.generate("add", T0, T0, "1");
	    Codegen.generateIndexed("sw", T0, T1, 0);
	}
    }

    /** unparse **/
    public void unparse(PrintWriter p, int indent) {
	p.print("++");
	myId.unparse(p,0);
	p.println(";");
    }

    // 1 kid
    private IdNode myId;
}

class PreDecStmtNode extends StmtNode {
    public PreDecStmtNode(IdNode id) {
	myId = id;
    }

    /** processNames **/
    public void processNames(SymTab S) {
	myId.processNames(S);
    }

    /** typeCheck **/
    public void typeCheck(String retType) {
	String T = myId.typeCheck();
	if (!isErrType(T)) {
	    if (!isIntType(T)) {
		Errors.fatal(myId.linenum(), myId.charnum(),
			     "Non-int identifier used with ++ or --");
	    }
	}
    }

    /** codeGen **/
    public void codeGen() {
	Codegen.genComment("PREDEC");
	// push address of id onto stack
	myId.genAddr();

	// evaluate rhs
	myId.codeGen();
	
	// load value into register, increment, then store
	if (isIntType(myId.type())) {
	    Codegen.genPop(T0, 4); // get id val
	    Codegen.genPop(T1, 4); // get id address
	    Codegen.generate("subu", T0, T0, "1");
	    Codegen.generateIndexed("sw", T0, T1, 0);
	}
    }

    /** unparse **/
    public void unparse(PrintWriter p, int indent) {
	p.print("--");
	myId.unparse(p,0);
	p.println(";");
    }

    // 1 kid
    private IdNode myId;
}

class PostIncStmtNode extends StmtNode {
    public PostIncStmtNode(IdNode id) {
	myId = id;
    }

    /** processNames **/
    public void processNames(SymTab S) {
	myId.processNames(S);
    }

    /** typeCheck **/
    public void typeCheck(String retType) {
	String T = myId.typeCheck();
	if (!isErrType(T)) {
	    if (!isIntType(T)) {
		Errors.fatal(myId.linenum(), myId.charnum(),
			     "Non-int identifier used with ++ or --");
	    }
	}
    }

    /** codeGen **/
    public void codeGen() {
	/** only accepts ints **/
	Codegen.genComment("POSTINC");
	// push address of id onto stack
	myId.genAddr();

	// evaluate rhs
	myId.codeGen();
	
	// load value into register, increment, then store
	Codegen.genPop(T0, 4); // get id val
	Codegen.genPop(T1, 4); // get id address
	Codegen.generate("add", T0, T0, "1");
	Codegen.generateIndexed("sw", T0, T1, 0);
    }

    /** unparse **/
    public void unparse(PrintWriter p, int indent) {
	myId.unparse(p,0);
	p.println("++;");
    }

    // 1 kid
    private IdNode myId;
}

class PostDecStmtNode extends StmtNode {
    public PostDecStmtNode(IdNode id) {
	myId = id;
    }

    /** processNames **/
    public void processNames(SymTab S) {
	myId.processNames(S);
    }

    /** typeCheck **/
    public void typeCheck(String retType) {
	String T = myId.typeCheck();
	if (!isErrType(T)) {
	    if (!isIntType(T)) {
		Errors.fatal(myId.linenum(), myId.charnum(),
			     "Non-int identifier used with ++ or --");
	    }
	}
    }
    
    /** codeGen **/
    public void codeGen() {
	/** only accepts ints **/

	Codegen.genComment("POSTDEC");
	// push address of id onto stack
	myId.genAddr();

	// evaluate rhs
	myId.codeGen();
	
	// load value into register, increment, then store
	Codegen.genPop(T0, 4); // get id val
	Codegen.genPop(T1, 4); // get id address
	Codegen.generate("subu", T0, T0, "1");
	Codegen.generateIndexed("sw", T0, T1, 0);
    }

    /** unparse **/
    public void unparse(PrintWriter p, int indent) {
	myId.unparse(p,0);
	p.println("--;");
    }

    // 1 kid
    private IdNode myId;
}

class ReadIntStmtNode extends StmtNode {
    public ReadIntStmtNode(IdNode id) {
	myId = id;
    }

    /** processNames **/
    public void processNames(SymTab S) {
	myId.processNames(S);
    }

    /** typeCheck **/
    public void typeCheck(String retType) {
	String T = myId.typeCheck();
	if (!isIntType(T)) {
	    Errors.fatal(myId.linenum(), myId.charnum(),
			 "Attempt to read a non-int id with an int format");
	}
    }

    /** codeGen **/
    public void codeGen() {
	Codegen.genComment("READ INT STMT");
	// get address of id node
	myId.genAddr();
	// pop address into register
	Codegen.genPop(T0, 4);

	// 5 for int
	Codegen.generate("li", V0, "5");
	// Syscall
	Codegen.generate("syscall");

	// store value
	Codegen.generateIndexed("sw", V0, T0, 0);

    }

    /** unparse **/
    public void unparse(PrintWriter p, int indent) {
	p.print("scanf(\"%d\", &");
	myId.unparse(p,0);
	p.println(");");
    }

    // 1 kid
    private IdNode myId;
}

class ReadDblStmtNode extends StmtNode {
    public ReadDblStmtNode(IdNode id) {
	myId = id;
    }

    /** processNames **/
    public void processNames(SymTab S) {
	myId.processNames(S);
    }

    /** typeCheck **/
    public void typeCheck(String retType) {
	String T = myId.typeCheck();
	if (!isDblType(T)) {
	    Errors.fatal(myId.linenum(), myId.charnum(),
			 "Attempt to read a non-double id with a dbl format");
	}
    }

    /** codeGen **/
    public void codeGen() {
	Codegen.genComment("READ DBL STMT");
	// get address of id node
	myId.genAddr();
	// pop address into register
	Codegen.genPop(T0, 4);

	// 7 for double
	Codegen.generate("li", V0, "7");
	// Syscall
	Codegen.generate("syscall");

	// store value
	Codegen.generateIndexed("s.d", F0, T0, 0);

    }

    /** unparse **/
    public void unparse(PrintWriter p, int indent) {
	p.print("scanf(\"%f\", &");
	myId.unparse(p,0);
	p.println(");");
    }

    // 1 kid
    private IdNode myId;
}

class WriteIntStmtNode extends StmtNode {
    public WriteIntStmtNode(ExpNode exp) {
	myExp = exp;
    }

    /** processNames **/
    public void processNames(SymTab S) {
	myExp.processNames(S);
    }

    /** typeCheck **/
    public void typeCheck(String retType) {
	String T = myExp.typeCheck();
	if (!isIntType(T) && !isErrType(T)) {
	    Errors.fatal(myExp.linenum(), myExp.charnum(),
			 "Attempt to write a non-int value with an int format");
	}
    }

    /** codeGen **/
    public void codeGen() {
	Codegen.genComment("WRITE INT STMT");
	// Put int to print on stack
	myExp.codeGen();
	// Pop into print register
	Codegen.genPop(A0, 4);
	// Load int syscall value
	Codegen.generate("li", V0, "1");
	// Syscall
	Codegen.generate("syscall");

	// check for post-inc/dec
	doPostGen();
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print("printf(\"%d\", ");
	myExp.unparse(p,0);
	p.println(");");
    }

    // 1 kid
    private ExpNode myExp;
}

class WriteDblStmtNode extends StmtNode {
    public WriteDblStmtNode(ExpNode exp) {
	myExp = exp;
    }

    /** processNames **/
    public void processNames(SymTab S) {
	myExp.processNames(S);
    }

    /** typeCheck **/
    public void typeCheck(String retType) {
	String T = myExp.typeCheck();
	if (!isDblType(T) && !isErrType(T)) {
	    Errors.fatal(myExp.linenum(), myExp.charnum(),
			 "Attempt to write a non-double value with a dbl format");
	}
    }

    /** codeGen **/
    public void codeGen() {
	Codegen.genComment("WRITE DBL STMT");
	// Put dbl to print on stack
	myExp.codeGen();
	if (myExp.returnSize() == 4)
	    intToDouble();
	// Pop into print register
	Codegen.genPop(F12, 8);
	// Load dbl syscall value
	Codegen.generate("li", V0, "3");
	// Syscall
	Codegen.generate("syscall");
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print("printf(\"%d\", ");
	myExp.unparse(p,0);
	p.println(");");
    }

    // 1 kid
    private ExpNode myExp;
}

class WriteStrStmtNode extends StmtNode {
    public WriteStrStmtNode(ExpNode exp) {
	myExp = exp;
    }

    /** processNames **/
    public void processNames(SymTab S) {
	/* only a stringliteral is possible, so no need to check */
    }

    /** typeCheck **/
    public void typeCheck(String retType) {
	/* only a stringliteral is possible, so no need to typecheck */
    }

    /** codeGen **/
    public void codeGen() {
	Codegen.genComment("WRITE STR STMT");
	// leave address of String on stack
	myExp.codeGen();

	// pop address into A0
	Codegen.genPop(A0, 4);

	// printString syscall
	Codegen.generateWithComment("li", V0, "4", "WRITE STR");
	Codegen.generate("syscall");
    }	

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print("printf(");
	myExp.unparse(p,0);
	p.println(");");
    }

    // 1 kid
    private ExpNode myExp;
}

class IfStmtNode extends StmtNode {
    public IfStmtNode(ExpNode exp, DeclListNode dlist, StmtListNode slist) {
	myDeclList = dlist;
	myExp = exp;
	myStmtList = slist;
    }

    /** typeCheck **/
    public void typeCheck(String retType) {
	String T = myExp.typeCheck();
	if (! isIntType(T) && ! isErrType(T)) {
	    Errors.fatal(myExp.linenum(), myExp.charnum(),
			 "Non-int expression used as an if condition");
	}
	myStmtList.typeCheck(retType);
    }

    /** processNames
     *  
     *  process the condition, then enter scope; process decls & stmts;
     *  exit scope
     *
     **/
    public void processNames(SymTab S) {
	myExp.processNames(S);
	S.addMap();
	myDeclList.processNames(S);
	myStmtList.processNames(S);
	try {
	    S.removeMap();
	} catch (EmptySymTabException ex) {
		System.err.println("unexpected EmptySymTabException in IfStmtNode.processNames");
		System.exit(-1);
	    }
    }

    /** codeGen **/
    public void codeGen() {
	Codegen.genComment("IF");
	// Evaluate expression - leave on stack
	myExp.codeGen();

	// check for post-inc/dec
	doPostGen();

	// Pop into register
	Codegen.genPop(T0, 4);
	// Generate label for exit
	String exit_lab = Codegen.nextLabel();
	// Generate conditional code
	Codegen.generate("beqz", T0, exit_lab);
	// Generate if code
	Codegen.genComment("-->IF");
	myStmtList.codeGen();
	// Generate Exit Label
	Codegen.genLabel(exit_lab, "EXIT IF");
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print("if (");
	myExp.unparse(p,0);
	p.println(") {");
	if (myDeclList != null) myDeclList.unparse(p,indent+2);
	if (myStmtList != null) myStmtList.unparse(p,indent+2);
	doIndent(p, indent);
	p.println("}");
    }

    // 3 kids
    private ExpNode myExp;
    private DeclListNode myDeclList;
    private StmtListNode myStmtList;
}

class IfElseStmtNode extends StmtNode {
    public IfElseStmtNode(ExpNode exp, DeclListNode dlist1,
			  StmtListNode slist1, DeclListNode dlist2,
			  StmtListNode slist2) {
	myExp = exp;
	myThenDeclList = dlist1;
	myThenStmtList = slist1;
	myElseDeclList = dlist2;
	myElseStmtList = slist2;
    }

    /** typeCheck **/
    public void typeCheck(String retType) {
	String T = myExp.typeCheck();
	if (! isIntType(T) && ! isErrType(T)) {
	    Errors.fatal(myExp.linenum(), myExp.charnum(),
			 "Non-int expression used as an if condition");
	}
	myThenStmtList.typeCheck(retType);
	myElseStmtList.typeCheck(retType);
    }

    /** processNames
     *  
     *  process the condition, then enter scope; process decls & stmts
     *  in "then" part; then exit scope; enter scope; process decls &
     *  stmts in "else" part; exit scope
     *
     **/
    public void processNames(SymTab S) {
	myExp.processNames(S);
	S.addMap();
	myThenDeclList.processNames(S);
	myThenStmtList.processNames(S);
	try {
	    S.removeMap();
	} catch (EmptySymTabException ex) {
		System.err.println("unexpected EmptySymTabException in IfElseStmtNode.processNames");
		System.exit(-1);
	    }
	S.addMap();
	myElseDeclList.processNames(S);
	myElseStmtList.processNames(S);
	try {
	    S.removeMap();
	} catch (EmptySymTabException ex) {
		System.err.println("unexpected EmptySymTabException in IfElseStmtNode.processNames");
		System.exit(-1);
	    }
    }

    /** codeGen **/
    public void codeGen() {
	Codegen.genComment("IF ELSE");
	// Evaluate Expression - leave on stack
	myExp.codeGen();

	// Check for post-inc/dec expressions
	doPostGen();

	// Pop into register
	Codegen.genPop(T0, 4);
	// Generate label for exit and true
	String exit_lab = Codegen.nextLabel();
	String true_lab = Codegen.nextLabel();
	// Generate Conditional code
	Codegen.generate("bnez", T0, true_lab);
	// Generate else code
	Codegen.genComment("-->ELSE");
	myElseStmtList.codeGen();
	// Jump to exit
	Codegen.generate("j", exit_lab);
	// Generate if code
	Codegen.genComment("-->IF");
	Codegen.genLabel(true_lab);
	myThenStmtList.codeGen();
	// Generate exit label
	Codegen.genLabel(exit_lab, "EXIT IFELSE");
	
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print("if (");
	myExp.unparse(p,0);
	p.println(") {");
	if (myThenDeclList != null) myThenDeclList.unparse(p,indent+2);
	if (myThenStmtList != null) myThenStmtList.unparse(p,indent+2);
	doIndent(p, indent);
	p.println("}");
	doIndent(p, indent);
	p.println("else {");
	if (myElseDeclList != null) myElseDeclList.unparse(p,indent+2);
	if (myElseStmtList != null) myElseStmtList.unparse(p,indent+2);
	doIndent(p, indent);
	p.println("}");
    }

    // 5 kids
    private ExpNode myExp;
    private DeclListNode myThenDeclList;
    private StmtListNode myThenStmtList;
    private StmtListNode myElseStmtList;
    private DeclListNode myElseDeclList;
}

class WhileStmtNode extends StmtNode {
    public WhileStmtNode(ExpNode exp, DeclListNode dlist, StmtListNode slist) {
	myExp = exp;
	myDeclList = dlist;
	myStmtList = slist;
    }

    /** typeCheck **/
    public void typeCheck(String retType) {
	String T = myExp.typeCheck();
	if (! isIntType(T) && ! isErrType(T)) {
	    Errors.fatal(myExp.linenum(), myExp.charnum(),
			 "Non-int expression used as a while condition");
	}
	myStmtList.typeCheck(retType);
    }

    /** processNames
     *  
     *  process the condition, then enter scope; process decls & stmts;
     *  exit scope
     *
     **/
    public void processNames(SymTab S) {
	myExp.processNames(S);
	S.addMap();
	myDeclList.processNames(S);
	myStmtList.processNames(S);
	try {
	    S.removeMap();
	} catch (EmptySymTabException ex) {
		System.err.println("unexpected EmptySymTabException in WhileStmtNode.processNames");
		System.exit(-1);
	    }
    }

    /** codeGen **/
    public void codeGen() {
	Codegen.genComment("WHILE LOOP");
	// Generate a label for condition evaluation
	String while_lab = Codegen.nextLabel();
	Codegen.genLabel(while_lab, "EVAL EXP");
	// Evaluate Expression - leave on stack
	myExp.codeGen();

	// Check for post-inc/dec expressions
	doPostGen();

	// Pop into register
	Codegen.genPop(T0, 4);
	// Generate label for exit
	String exit_lab = Codegen.nextLabel();
	// Generate conditional code
	Codegen.generate("beqz", T0, exit_lab);
	// Generate statement code
	/*myDeclList.codeGen();   <- Nothing to do here */
	myStmtList.codeGen();
	// Re-evaluate conditional
	Codegen.generate("j", while_lab);
	// Exit Lab
	Codegen.genLabel(exit_lab, "EXIT WHILE");
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print("while (");
	myExp.unparse(p,0);
	p.println(") {");
	if (myDeclList != null) myDeclList.unparse(p,indent+2);
	if (myStmtList != null) myStmtList.unparse(p,indent+2);
	doIndent(p, indent);
	p.println("}");
    }

    // 3 kids
    private ExpNode myExp;
    private DeclListNode myDeclList;
    private StmtListNode myStmtList;
}

class CallStmtNode extends StmtNode {
    public CallStmtNode(CallExpNode call) {
	myCall = call;
    }

    /** typeCheck **/
    public void typeCheck(String retType) {
	myCall.typeCheck();
    }

    /** processNames **/
    public void processNames(SymTab S) {
	myCall.processNames(S);
    }

    /** codeGen **/
    public void codeGen() {
	myCall.codeGen();

	// Pop value off of stack
	if (myCall.returnSize() == 4)
	    Codegen.genPop(V0, 4);
	else if (myCall.returnSize() == 8)
	    Codegen.genPop(F0, 8);

	// Check for post-inc/dec expressions
	doPostGen();
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	myCall.unparse(p,indent);
	p.println(";");
    }

    // 1 kid
    private CallExpNode myCall;
}

class ReturnStmtNode extends StmtNode {
    public ReturnStmtNode(ExpNode exp) {
	myExp = exp;
    }

    /** typeCheck **/
    public void typeCheck(String retType) {
	if (myExp != null) {
	    // return with a value
	    // error if
	    // (a) fn return type is void, or
	    // (b) value type is non-numeric, or
	    // (c) value type is dbl and return type is int
	    String T = myExp.typeCheck();
	    if (isVoidType(retType)) {
		Errors.fatal(myExp.linenum(), myExp.charnum(),
			     "Return with a value in a void function");
		return;
	    }
	    if (isErrType(T)) return;
	    if (! isNumericType(T) || (isDblType(T) && isIntType(retType))) {
		Errors.fatal(myExp.linenum(), myExp.charnum(),
			     "Bad return value");
	    }
	}
	else {
	    // return w/o value
	    // error if fn return type is NOT void
	    if (! isVoidType(retType)) {
		Errors.fatal(0, 0, "Missing return value");
	    }
	}
    }

    /** processNames **/
    public void processNames(SymTab S) {
	if (myExp != null) myExp.processNames(S);
    }

    /** codeGen **/
    public void codeGen() {
	Codegen.genComment("RTN STMT");
	// Check for null ExpNode
	if (myExp != null) {
	    // Evaluate expression
	    myExp.codeGen();
	    
	    // Check return type
	    if (myExp.returnSize() == 4) { // int return type
		if (isDblType(FN_RETURN)) { // check if we need to convert
		    intToDouble();
		    Codegen.genPop(F0, 8);
		} else {
		    // place in return reg
		    Codegen.genPop(V0, 4);
		}

	    } else if (myExp.returnSize() == 8)  { // dbl return type
		// place in return reg
		Codegen.genPop(F0, 8);

	    } else { // void return
	    }
	    
	}
	
	// Check for post-inc/dec expressions
	doPostGen();
	
	// return
	Codegen.generateWithComment("j", FN_EXIT, "RTN");	
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print("return");
	if (myExp != null) {
	    p.print(" ");
	    myExp.unparse(p,0);
	}
	p.println(";");
    }

    // 1 kid
    private ExpNode myExp;
}

// **********************************************************************
// ExpNode and its subclasses
// **********************************************************************

abstract class ExpNode extends ASTnode {
    // default version of processNames (for nodes with no names)
    public void processNames(SymTab S) {}

    // return size for codeGen
    public int returnSize() { return returnSize; }
    protected int returnSize;

    abstract public void codeGen();
    abstract public String typeCheck();
    abstract public int linenum();
    abstract public int charnum();
}

class IntLitNode extends ExpNode {
    public IntLitNode(int lineNum, int charNum, int intVal) {
	myLineNum = lineNum;
	myCharNum = charNum;
	myIntVal = intVal;
	returnSize = 4;
    }

    /** typeCheck **/
    public String typeCheck() {
	return INT_TYPE;
    }

    /** codeGen **/
    public void codeGen() {
	// Load literal into reg
	Codegen.generateWithComment("li", T0, Integer.toString(myIntVal), "INTLIT");
	// Push onto stack
	Codegen.genPush(T0, 4);
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print(myIntVal);
    }

    /** linenum **/
    public int linenum() {
	return myLineNum;
    }

    /** charnum **/
    public int charnum() {
	return myCharNum;
    }

    private int myLineNum;
    private int myCharNum;
    private int myIntVal;
}

class DblLitNode extends ExpNode {
    public DblLitNode(int lineNum, int charNum, double dblVal) {
	myLineNum = lineNum;
	myCharNum = charNum;
	myDblVal = dblVal;
	returnSize = 8;
    }

    /** typeCheck **/
    public String typeCheck() {
	return DBL_TYPE;
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print(myDblVal);
    }

    /** codeGen **/
    public void codeGen() {
	// Load literal into reg
	Codegen.generateWithComment("li.d", F0, Double.toString(myDblVal), "DBLLIT");
	// Push onto stack
	Codegen.genPush(F0, 8);
    }

    /** linenum **/
    public int linenum() {
	return myLineNum;
    }

    /** charnum **/
    public int charnum() {
	return myCharNum;
    }

    private int myLineNum;
    private int myCharNum;
    private double myDblVal;
}

class StringLitNode extends ExpNode {
    public StringLitNode(int lineNum, int charNum, String strVal) {
	myLineNum = lineNum;
	myCharNum = charNum;
	myStrVal = strVal;
	returnSize = 4;
    }

    /** typeCheck **/
    public String typeCheck() {
	return "string";
    }

    /** codeGen **/
    public void codeGen() {
	String label = labelMap.get(myStrVal);

	// Check if string doesn't exist
	if (label == null) {
	    // Create a new label	  
	    label = Codegen.nextLabel();
	    // Add to map
	    labelMap.put(myStrVal, label);
	    // Generate label code
	    Codegen.generate(".data", "");
	    Codegen.generate(".align 2", "");
	    Codegen.genLabel(label);
	    Codegen.generate(".asciiz", myStrVal);
	    Codegen.generate(".text", "");
	}

	// Load address of label
	Codegen.generate("la", T0, label);
	Codegen.genPush(T0, 4);
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print(myStrVal);
    }

    /** linenum **/
    public int linenum() {
	return myLineNum;
    }

    /** charnum **/
    public int charnum() {
	return myCharNum;
    }

    
    private int myLineNum;
    private int myCharNum;
    private String myStrVal;
}

class IdNode extends ExpNode {
    public IdNode(int lineNum, int charNum, String strVal) {
	myLineNum = lineNum;
	myCharNum = charNum;
	myStrVal = strVal;
    }

    /** typeCheck **/
    public String typeCheck() {
	if (mySym != null) {
	    if (isIntType(mySym.returnType()))  { // INT
		returnSize = 4;
	    } else if (isDblType(mySym.returnType())) {  // DBL
		returnSize = 8;
	    } else {
		returnSize = 4;
	    }
	    return mySym.type();
	}
	else {
	    System.err.println("ID with null sym field in IdNode.typeCheck");
	    System.exit(-1);
	}
	return null;
    }

    /** codeGen **/
    public void codeGen() {
	/** Need to leave id value on stack **/
	if (isIntType(this.type())) {
	    // Load address
	    if (mySym.isLocal()) { // is local
		Codegen.generateIndexed("lw", T0, FP, -mySym.offset(), "LW " + myStrVal);
	    } else { // is global
		Codegen.generate("lw", T0, "_" + this.name());
	    }
	    // push onto stack
	    Codegen.genPush(T0, 4);
	} else if (isDblType(this.type())) {
	    // ld from stack
	    if (mySym.isLocal()) { // is local
		Codegen.generateIndexed("l.d", F0, FP, -mySym.offset(), "LW " + myStrVal);
	    } else { // is global
		Codegen.generate("l.d", F0, "_" + this.name());
	    }
	    // push onto stack
	    Codegen.genPush(F0, 8);
	}	    
    }

    /** genAddr **/
    public void genAddr() {
	/** Need to leave id address on stack **/
	if (mySym.isLocal()) {
	    Codegen.generateIndexed("la", T0, FP, -mySym.offset(), "LA " + myStrVal);
	    Codegen.genPush(T0, 4);
	} else { // is global
	    Codegen.generate("la", T0, "_" + this.name());
	    Codegen.genPush(T0, 4);
	}
    }

    /** genJal **/
    public void genJAL() {
	if (myStrVal.equals("main")) {
	    Codegen.generate("jal", "main");
	} else {
	    Codegen.generate("jal", "_" + myStrVal);
	}
    }

    /** processNames
     *
     * check for use of an undeclared name
     * if OK, link to symtab entry
     *
     **/
    public void processNames(SymTab S) {
	Sym sym = S.globalLookup(myStrVal);
	if (sym  == null) {
	    Errors.fatal(myLineNum, myCharNum,
			 "Undeclared identifier");
	} else {
	    link(sym);
	}
    }

    /** link **/
    public void link(Sym sym) {
	mySym = sym;
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print(myStrVal);
	p.print("(" + mySym.type() + ")");
    }

    /** name **/
    public String name() {
	return myStrVal;
    }

    /** type **/
    public String type() {
	if (mySym != null) return mySym.type();
	else {
	    System.err.println("ID with null sym field");
	    System.exit(-1);
	}
	return null;
    }

    /** symbol-table entry */
    public Sym sym() {
	return mySym;
    }

    /** line num **/
    public int linenum() {
	return myLineNum;
    }

    /** char num **/
    public int charnum() {
	return myCharNum;
    }

    // fields
    private int myLineNum;
    private int myCharNum;
    private String myStrVal;
    private Sym mySym;
}

class CallExpNode extends ExpNode {
    public CallExpNode(IdNode name, ExpListNode elist) {
	myId = name;
	myExpList = elist;
    }

    /** typeCheck **/
    public String typeCheck() {
	String T = myId.typeCheck();
	// check that ID is a fn
	if (! isFnType(T)) {
	    Errors.fatal(myId.linenum(), myId.charnum(),
			 "Attempt to call a non-function");
	    return ERR_TYPE;
	}

	// check number of args
	FnSym s = (FnSym)myId.sym();
	if (s == null) {
	    System.out.println("null sym for ID in CallExpNode.typeCheck");
	    System.exit(-1);
	}

	int numParams = s.numparams();
	if (numParams != myExpList.length()) {
	    Errors.fatal(myId.linenum(), myId.charnum(),
			 "Function call with wrong number of args");
	    return s.returnType();
	}

	// check type of each arg
	myExpList.typeCheck(s.paramTypes());

	// set returnType
	if (isIntType(s.returnType()))
	    returnSize = 4;
	else
	    returnSize = 8;
	return s.returnType();
    }

    /** processNames 
     *
     * process name of called fn and all actuals
     **/
    public void processNames(SymTab S) {
	myId.processNames(S);
	myExpList.processNames(S);
    }
    
    /** codeGen **/
    public void codeGen() {
	/** myExpList needs to know size of expected parameters
	    for intToDouble conversion.  Therefore, we set a static
	    list of types for comparision **/
	paramTypes = ((FnSym)myId.sym()).paramTypes();
	
	// Evaluate expressions - leave on stack
	myExpList.codeGen();
	// Generate JAL to function call
	myId.genJAL();
	// Return value will be on the stack
	if (myId.returnSize == 4) {  // INT RETURNED
	    // No conversion
	    Codegen.genPush(V0, 4);
	}
	
	else if (myId.returnSize == 8) { //DBL
	    Codegen.genPush(F0, 8);
	}
    }
    
    
    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	myId.unparse(p,0);
	p.print("(");
	if (myExpList != null) myExpList.unparse(p,0);
	p.print(")");
    }

    /** linenum **/
    public int linenum() {
	return myId.linenum();
    }

    /** charnum **/
    public int charnum() {
	return myId.charnum();
    }

    // 2 kids
    private IdNode myId;
    private ExpListNode myExpList;
}

abstract class UnaryExpNode extends ExpNode {
    public UnaryExpNode(ExpNode exp) {
	myExp = exp;
    }

    /** processNames **/
    public void processNames(SymTab S) {
	myExp.processNames(S);
    }

    /** codeGen **/
    public void codeGen() {}

    /** linenum **/
    public int linenum() {
	return myExp.linenum();
    }

    /** charnum **/
    public int charnum() {
	return myExp.charnum();
    }

    // one kid
    protected ExpNode myExp;
}

abstract class BinaryExpNode extends ExpNode {
    public BinaryExpNode(ExpNode exp1, ExpNode exp2) {
	myExp1 = exp1;
	myExp2 = exp2;
    }

    /** processNames **/
    public void processNames(SymTab S) {
	myExp1.processNames(S);
	myExp2.processNames(S);
    }

    /** codeGen **/
    public void codeGen() {}

    /** linenum **/
    public int linenum() {
	return myExp1.linenum();
    }

    /** charnum **/
    public int charnum() {
	return myExp1.charnum();
    }

    // two kids
    protected ExpNode myExp1;
    protected ExpNode myExp2;
}

// **********************************************************************
// Subclasses of UnaryExpNode
// **********************************************************************

class PlusPlusNode extends UnaryExpNode {
    public PlusPlusNode(IdNode id) {
	super(id);
    }

    /** typeCheck **/
    public String typeCheck() {
	String T = myExp.typeCheck();
	
	// Set returnSize for codeGen
	if (isIntType(T))
	    returnSize = 4;
	else
	    returnSize = 8;

	if (! isErrType(T)) {
	    if (! isIntType(T)) {
		Errors.fatal(myExp.linenum(), myExp.charnum(),
			     "Non-int identifier used with ++ or --");
		return ERR_TYPE;
	    }
	    return INT_TYPE;
	} else return T;
    }

    /** codeGen **/
    public void codeGen() {
	// Need to add to list of post-inc ids
	postIncList.add(new PostIncStmtNode((IdNode)myExp));

	// Need to leave current value on stack
	myExp.codeGen();
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print("(");
	myExp.unparse(p, 0);
	p.print("++");
	p.print(")");
    }
}

class MinusMinusNode extends UnaryExpNode {
    public MinusMinusNode(IdNode id) {
	super(id);
    }

    /** typeCheck **/
    public String typeCheck() {
	String T = myExp.typeCheck();

	// Must set returnSize for codeGen
	if (isIntType(T))
	    returnSize = 4;
	else
	    returnSize = 8;

	if (! isErrType(T)) {
	    if (! isIntType(T)) {
		Errors.fatal(myExp.linenum(), myExp.charnum(),
			     "Non-int identifier used with ++ or --");
		return ERR_TYPE;
	    }
	    return INT_TYPE;
	} else return T;
    }

    public void codeGen() {
	// Need to add to list of post-dec ids
	postDecList.add(new PostDecStmtNode((IdNode)myExp));

	// Need to leave current value on stack
	myExp.codeGen();
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print("(");
	myExp.unparse(p, 0);
	p.print("--");
	p.print(")");
    }
}

class UnaryMinusNode extends UnaryExpNode {
    public UnaryMinusNode(ExpNode exp) {
	super(exp);
    }

    /** typeCheck **/
    public String typeCheck() {
	String T = myExp.typeCheck();
	// Must set returnSize for codeGen
	if (isIntType(T))
	    returnSize = 4;
	else
	    returnSize = 8;


	if (! isNumericType(T) && ! isErrType(T)) {
	    Errors.fatal(myExp.linenum(), myExp.charnum(),
			 "Illegal use of non-numeric operand");
	    return ERR_TYPE;
	}
	return T;
    }

   /** codeGen **/
    public void codeGen() {
	// Push value onto stack
	myExp.codeGen();

	// Check size of value
	if (myExp.returnSize() == 4) { //INT
	    Codegen.genPop(T0, 4);
	    // Invert the value
	    Codegen.generate("sub", T0, "$0", T0);
	    // Push results
	    Codegen.genPush(T0, 4);
	} else { // DBL
	    Codegen.genPop(F0, 8);
	    // Zero a double reg
	    Codegen.generate("sub.d", F2, F2, F2);
	    // Invert
	    Codegen.generate("sub.d", F0, F2, F0);
	    // Push result
	    Codegen.genPush(F0, 8);
	}
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print("(-");
	myExp.unparse(p, 0);
	p.print(")");
    }
}

class NotNode extends UnaryExpNode {
    public NotNode(ExpNode exp) {
	super(exp);
    }

    /** typeCheck **/
    public String typeCheck() {
	String T = myExp.typeCheck();

	// Must set returnSize for codeGen
	if (isIntType(T))
	    returnSize = 4;
	else
	    returnSize = 8;

	if (! isIntType(T) && ! isErrType(T)) {
	    Errors.fatal(myExp.linenum(), myExp.charnum(),
			 "Logical operator applied to non-int operand");
	    return ERR_TYPE;
	}
	return T;
    }

    /** codeGen **/
    public void codeGen() {
	// Evaluate expression - leaves an int on the stack
	myExp.codeGen();
	
	// Pop into reg
	Codegen.genPop( T0, 4 );
	// Check condition
	String false_lab = Codegen.nextLabel();
	String exit_lab = Codegen.nextLabel();
	Codegen.generate("bnez", T0, false_lab);
	// True condition
	Codegen.generate("li", T0, "1");
	Codegen.generate("j", exit_lab);
	// False condition
	Codegen.genLabel(false_lab);
	Codegen.generate("li", T0, "0");
	Codegen.genLabel(exit_lab);
	Codegen.genPush( T0, 4);
    }
    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print("(!");
	myExp.unparse(p, 0);
	p.print(")");
    }
}

// **********************************************************************
// Subclasses of BinaryExpNode
// **********************************************************************

class AssignNode extends BinaryExpNode {
    private static final boolean DEBUG = false;
    
    public AssignNode(ExpNode lhs, ExpNode exp) {
	super(lhs, exp);
    }

    public void codeGen() {
	// put RHS on stack
	myExp2.codeGen();
	// Convert if necessary
	if (returnSize == 8 && myExp2.returnSize() == 4)
	    intToDouble();
	// put ID address on stack
	((IdNode)myExp1).genAddr();
	
	if (returnSize == 8) { // double on stack
	    // Pop address into T0
	    Codegen.genPop(T0, 4);
	    // Pop value into F0
	    Codegen.genPop(F0, 8);
	    // Store F0 into id's address
	    Codegen.generateIndexed("s.d", F0, T0, 0);
	    // Leave value on stack
	    Codegen.genPush(F0, 8);
	} else if (returnSize == 4) { // int on stack
	    // Pop address into T0
	    Codegen.genPop(T0, 4);
	    // Pop value into T1
	    Codegen.genPop(T1, 4);
	    // Store T1 into id's address
	    Codegen.generateIndexed("sw", T1, T0, 0);
	    // Leave value on stack
	    Codegen.genPush(T1, 4);
	}
    }

    /** typeCheck **/
    public String typeCheck() {
	String retType;
	String T1 = myExp1.typeCheck();
	String T2 = retType = myExp2.typeCheck();
	if (! isNumericType(T1) && ! isErrType(T1)) {
	    Errors.fatal(myExp1.linenum(), myExp1.charnum(),
			 "Illegal use of non-numeric operand");
	    T1 = ERR_TYPE;
	}
	if (! isNumericType(T2) && ! isErrType(T2)) {
	    Errors.fatal(myExp2.linenum(), myExp2.charnum(),
			 "Illegal use of non-numeric operand");
	    return ERR_TYPE;
	}
	if (isErrType(T1) || isErrType(T2)) return ERR_TYPE;
	if (isIntType(T1) && isDblType(T2)) {
	    Errors.fatal(myExp2.linenum(), myExp2.charnum(),
			 "Possible loss of precision");
	    return ERR_TYPE;
	}

	// must set returnSize for codeGen
	if (isDblType(T1) || isDblType(T2)) 
	    returnSize = 8; // one is a double
	else
	    returnSize = 4; // both are ints
	return T1;
    }

    // ** unparse **
    //
    // Two versions: One called from the unparse method of
    // AssignStmtNode -- do NOT enclose this assignment in parens;
    // The other called whenever this assignment really is an
    // expression, not a stmt, so DO enclose this assignment in
    // parens.
    
    public void unparse(PrintWriter p, int indent) {
	p.print("(");
	myExp1.unparse(p, 0);
	p.print(" = ");
	myExp2.unparse(p,0);
	p.print(")");
    }

    /** processNames **/
    public void processNames(SymTab S) {
	myExp1.processNames(S);
	myExp2.processNames(S);
    }

    public void unparse(PrintWriter p, int indent, boolean b) {
	myExp1.unparse(p, 0);
	p.print(" = ");
	myExp2.unparse(p,0);
    }
}

abstract class ArithmeticBinExpNode extends BinaryExpNode {
    public ArithmeticBinExpNode(ExpNode exp1, ExpNode exp2) {
	super(exp1, exp2);
    }

    /** typeCheck **/
    public String typeCheck() {
	String T1 = myExp1.typeCheck();
	String T2 = myExp2.typeCheck();
	if (! isNumericType(T1) && ! isErrType(T1)) {
	    Errors.fatal(myExp1.linenum(), myExp1.charnum(),
			 "Illegal use of non-numeric operand");
	}
	if (! isNumericType(T2) && ! isErrType(T2)) {
	    Errors.fatal(myExp2.linenum(), myExp2.charnum(),
			 "Illegal use of non-numeric operand");
	    return ERR_TYPE;
	}
	if (isErrType(T1) || isErrType(T2)) return ERR_TYPE;
	else {
	    if (isDblType(T1) || isDblType(T2)) {
		returnSize = 8;
		return DBL_TYPE;
	    }
	    else {
		returnSize = 4;
		return INT_TYPE;
	    }
	}
    }

    /** codeGen **/
    public void codeGen(String op) {
	/** Accepted ops:
	    mul, add, sub, div
	**/

	// Need to leave resulting expression on stack
	if (myExp1.returnSize == 4 && myExp2.returnSize == 4) { // both are ints
	    // Evaluate both expressions
	    myExp1.codeGen();
	    myExp2.codeGen();
	    // Pop values
	    Codegen.genPop(T1, 4);
	    Codegen.genPop(T0, 4);
	    // Evaluate and push
	    Codegen.generate( op, T0, T0, T1 );
	    Codegen.genPush( T0, 4);
	} else if (myExp1.returnSize == 8 && myExp2.returnSize == 8) { // both are dbl
	    // Evaluate both expressions
	    myExp1.codeGen();
	    myExp2.codeGen();
	    // Pop values
	    Codegen.genPop(F2, 8);
	    Codegen.genPop(F0, 8);
	    // Evaluate and push
	    Codegen.generate( op + ".d", F0, F0, F2 );
	    Codegen.genPush( F0, 8);
	}
	else { // MIXED PARAMS
	    // Promote the int to double
	    if (myExp1.returnSize == 4) {
		myExp1.codeGen();
		intToDouble();
		myExp2.codeGen();
	    } else {
		myExp1.codeGen();
		myExp2.codeGen();
		intToDouble();
	    }
	    // Pop values
	    Codegen.genPop(F2, 8);
	    Codegen.genPop(F0, 8);
	    // Evaluate and push
	    Codegen.generate( op + ".d", F0, F0, F2 );
	    Codegen.genPush( F0, 8);
	}
    }
}

abstract class EqualityBinExpNode extends BinaryExpNode {
    public EqualityBinExpNode(ExpNode exp1, ExpNode exp2) {
	super(exp1, exp2);
    }

    /** typeCheck **/
    public String typeCheck() {
	String T1 = myExp1.typeCheck();
	String T2 = myExp2.typeCheck();
	if (! isNumericType(T1) && ! isErrType(T1)) {
	    Errors.fatal(myExp1.linenum(), myExp1.charnum(),
			 "Illegal use of non-numeric operand");
	    T1 = ERR_TYPE;
	}
	if (! isNumericType(T2) && ! isErrType(T2)) {
	    Errors.fatal(myExp2.linenum(), myExp2.charnum(),
			 "Illegal use of non-numeric operand");
	    return ERR_TYPE;
	}
	if (isErrType(T1) || isErrType(T2)) {
	    returnSize = 8;
	    return ERR_TYPE;
	}
	else {
	    returnSize = 4;
	    return INT_TYPE;
	}
    }

    public static void genLessWithDbl(ExpNode exp1, ExpNode exp2) {
	String true_lab = Codegen.nextLabel();
	String false_lab = Codegen.nextLabel();
	String exit_lab = Codegen.nextLabel();
	
	/** Compare sign, exponent, and lsb **/
	// Push both values on the stack
	exp1.codeGen();
	if (exp1.returnSize() == 4) { // Int->Dbl
	    intToDouble();
	}
	exp2.codeGen();
	if (exp2.returnSize() == 4) { // Int->Dbl
	    intToDouble();
	}
	
	// Pop into registers
	Codegen.genPop(T1, 4);  // LSB2
	Codegen.genPop(T0, 4);  // MSB2
	Codegen.genPop(T3, 4);  // LSB1
	Codegen.genPop(T2, 4);  // MSB1

	/** Compare the signs of each **/
	Codegen.generate("srl", T4, T0, 31);
	Codegen.generate("srl", T5, T2, 31);
	// If LHS sign = 1 and RHS = 0, TRUE
	Codegen.generate("blt", T4, T5, true_lab);
	// If LHS sign = 0 and RHS = 1, FALSE
	Codegen.generate("bgt", T4, T5, false_lab);


	/** Sign must be the same - Compare exponent sign **/
	Codegen.generate("srl", T4, T0, 30);
	Codegen.generate("srl", T5, T2, 30);
	Codegen.generate("bgt", T4, T5, true_lab);
	Codegen.generate("blt", T4, T5, false_lab);

	/** Exponent sign must be same - Compare exponent **/
	Codegen.generate("srl", T4, T0, 20);
	Codegen.generate("srl", T5, T2, 20);
	Codegen.generate("bgt", T4, T5, true_lab);
	Codegen.generate("blt", T4, T5, false_lab);

	/** Exponent must be the same - Compare MSB fractional **/
	Codegen.generate("and", T4, T0, "0x000FFFFF"); // Mask exponent
	Codegen.generate("and", T5, T2, "0x000FFFFF");
	Codegen.generate("bgt", T4, T5, true_lab);  // Check fractional
	Codegen.generate("blt", T4, T5, false_lab); 

	/** MSB must be the same - Compare LSB **/
	Codegen.generate("blt", T3, T1, true_lab);


	/** FALSE **/
	Codegen.genLabel(false_lab);
	Codegen.generate("li", T0, "0");
	Codegen.generate("j", exit_lab);
	/** TRUE **/
	Codegen.genLabel(true_lab);
	Codegen.generate("li", T0, "1");
	/** EXIT **/
	Codegen.genLabel(exit_lab);
	Codegen.genPush(T0, 4);
    }

    public static void genLessEqWithDbl(ExpNode exp1, ExpNode exp2) {
	String true_lab = Codegen.nextLabel();
	String false_lab = Codegen.nextLabel();
	String exit_lab = Codegen.nextLabel();
	
	/** Compare sign, exponent, and lsb **/
	// Push both values on the stack
	exp1.codeGen();
	if (exp1.returnSize() == 4) {
	    intToDouble();
	}
	exp2.codeGen();
	if (exp2.returnSize() == 4) {
	    intToDouble();
	}
	
	// Pop into registers
	Codegen.genPop(T1, 4);  // LSB2
	Codegen.genPop(T0, 4);  // MSB2
	Codegen.genPop(T3, 4);  // LSB1
	Codegen.genPop(T2, 4);  // MSB1

	/** Compare the signs of each **/
	Codegen.generate("srl", T4, T0, 31);
	Codegen.generate("srl", T5, T2, 31);
	// If LHS sign = 1 and RHS = 0, TRUE
	Codegen.generate("blt", T4, T5, true_lab);
	// If LHS sign = 0 and RHS = 1, FALSE
	Codegen.generate("bgt", T4, T5, false_lab);


	/** Sign must be the same - Compare exponent sign **/
	Codegen.generate("srl", T4, T0, 30);
	Codegen.generate("srl", T5, T2, 30);
	Codegen.generate("bgt", T4, T5, true_lab);
	Codegen.generate("blt", T4, T5, false_lab);

	/** Exponent sign must be same - Compare exponent **/
	Codegen.generate("srl", T4, T0, 20);
	Codegen.generate("srl", T5, T2, 20);
	Codegen.generate("bgt", T4, T5, true_lab);
	Codegen.generate("blt", T4, T5, false_lab);

	/** Exponent must be the same - Compare MSB fractional **/
	Codegen.generate("and", T4, T0, "0x000FFFFF"); // Mask exponent
	Codegen.generate("and", T5, T2, "0x000FFFFF");
	Codegen.generate("bgt", T4, T5, true_lab);  // Check fractional
	Codegen.generate("blt", T4, T5, false_lab); 

	/** MSB must be the same - Compare LSB **/
	Codegen.generate("blt", T3, T1, true_lab);
	Codegen.generate("bgt", T3, T1, false_lab);

	/** Must be equal **/
	Codegen.generate("j", true_lab);

	/** FALSE **/
	Codegen.genLabel(false_lab);
	Codegen.generate("li", T0, "0");
	Codegen.generate("j", exit_lab);
	/** TRUE **/
	Codegen.genLabel(true_lab);
	Codegen.generate("li", T0, "1");
	/** EXIT **/
	Codegen.genLabel(exit_lab);
	Codegen.genPush(T0, 4);
    }

    /** codeGen **/
    public void codeGen(String op) {
	/** Accepted ops:
	    beq   bne   bgt   bge   
	    blt   ble  
	**/

	/* Evaluate LHS and RHS - leave on stack **/
	myExp1.codeGen();
	myExp2.codeGen();

	/* Determine size of operands */
	if (myExp1.returnSize == 4 && myExp2.returnSize == 4) { // both are ints
	    // Pop into registers
	    Codegen.genPop(T1, 4); //RHS
	    Codegen.genPop(T0, 4); //LHS
	    // Generate label for true and exit
	    String true_lab = Codegen.nextLabel();
	    String exit_lab = Codegen.nextLabel();
	    // Generate conditional code
	    Codegen.generate(op, T0, T1, true_lab);
	    // Generate code for false condition
	    Codegen.generate("li", T0, 0);
	    Codegen.generate("j", exit_lab);
	    // Generate code for true condition
	    Codegen.genLabel(true_lab, "TRUE");
	    Codegen.generate("li", T0, 1);
	    // Push result
	    Codegen.genLabel(exit_lab, "EXIT");
	    Codegen.genPush(T0, 4);
	    
	}
    }	
}

abstract class LogicalBinExpNode extends BinaryExpNode {
    public LogicalBinExpNode(ExpNode exp1, ExpNode exp2) {
	super(exp1, exp2);
    }

    /** typeCheck **/
    public String typeCheck() {
	// return is always an int
	returnSize = 4;

	String T1 = myExp1.typeCheck();
	String T2 = myExp2.typeCheck();
	String retType = INT_TYPE;
	if (! isIntType(T1) && ! isErrType(T1)) {
	    Errors.fatal(myExp1.linenum(), myExp1.charnum(),
			 "Logical operator applied to non-int operand");
	    retType = ERR_TYPE;
	}
	if (! isIntType(T2) && ! isErrType(T2)) {
	    Errors.fatal(myExp2.linenum(), myExp2.charnum(),
			 "Logical operator applied to non-int operand");
	    retType = ERR_TYPE;
	}
	if (isErrType(T1) || isErrType(T2)) return ERR_TYPE;
	else return retType;
    }

}

// **********************************************************************
// Subclasses of ArithmeticBinExpNode
// **********************************************************************

class PlusNode extends ArithmeticBinExpNode {
    public PlusNode(ExpNode exp1, ExpNode exp2) {
	super(exp1, exp2);
    }

    /** codeGen **/
    public void codeGen() {
	Codegen.genComment("ADD");
	((ArithmeticBinExpNode)this).codeGen("add");
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print("(");
	myExp1.unparse(p, 0);
	p.print("+");
	myExp2.unparse(p, 0);
	p.print(")");
    }
}

class MinusNode extends ArithmeticBinExpNode {
    public MinusNode(ExpNode exp1, ExpNode exp2) {
	super(exp1, exp2);
    }

    /** codeGen **/
    public void codeGen() {
	Codegen.genComment("SUB");
	((ArithmeticBinExpNode)this).codeGen("sub");
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print("(");
	myExp1.unparse(p, 0);
	p.print("-");
	myExp2.unparse(p, 0);
	p.print(")");
    }
}

class TimesNode extends ArithmeticBinExpNode {
    public TimesNode(ExpNode exp1, ExpNode exp2) {
	super(exp1, exp2);
    }

    /** codeGen **/
    public void codeGen() {
	Codegen.genComment("MUL");
	((ArithmeticBinExpNode)this).codeGen("mul");
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print("(");
	myExp1.unparse(p, 0);
	p.print("*");
	myExp2.unparse(p, 0);
	p.print(")");
    }
}

class DivideNode extends ArithmeticBinExpNode {
    public DivideNode(ExpNode exp1, ExpNode exp2) {
	super(exp1, exp2);
    }

    /** codeGen **/
    public void codeGen() {
	Codegen.genComment("DIV");
	((ArithmeticBinExpNode)this).codeGen("div");
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print("(");
	myExp1.unparse(p, 0);
	p.print("/");
	myExp2.unparse(p, 0);
	p.print(")");
    }
}

class EqualsNode extends EqualityBinExpNode {
    public EqualsNode(ExpNode exp1, ExpNode exp2) {
	super(exp1, exp2);
    }

    /** codeGen **/
    public void codeGen() {
	Codegen.genComment("EQ");
	if (myExp1.returnSize() == 4 && myExp2.returnSize() == 4) // INTs
	    ((EqualityBinExpNode)this).codeGen("beq");
	else {  // At least one is an int
	    /** Subtracting both doubles will be 0 if equal **/
	    // Evaluate and convert if necessary
	    myExp1.codeGen();
	    if (myExp1.returnSize() == 4)
		intToDouble();
	    
	    myExp2.codeGen();
	    if (myExp2.returnSize() == 4)
		intToDouble();
	    // Pop
	    Codegen.genPop(F0, 8);
	    Codegen.genPop(F2, 8);
	    // Subtract
	    Codegen.generate("sub.d", F0, F0, F2);
	    // Push onto stack
	    Codegen.genPush(F0, 8);

	    // Pop off MSB and LSB
	    Codegen.genPop(T0, 4);
	    Codegen.genPop(T1, 4);

	    // Force sign bit 0
	    Codegen.generate("and", T0, T0, "0x7FFFFFFF");

	    String false_lab = Codegen.nextLabel();
	    String true_lab = Codegen.nextLabel();
	    String exit_lab = Codegen.nextLabel();
	    
	    // Check values
	    Codegen.generate("bnez", T0, false_lab);
	    Codegen.generate("bnez", T1, false_lab);
	    
	    // TRUE
	    Codegen.genLabel(true_lab);
	    Codegen.generate("li", T0, 1);
	    Codegen.generate("j", exit_lab);

	    // FALSE
	    Codegen.genLabel(false_lab);
	    Codegen.generate("li", T0, 0);

	    // EXIT
	    Codegen.genLabel(exit_lab);
	    Codegen.genPush(T0, 4);
	}
	    
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print("(");
	myExp1.unparse(p, 0);
	p.print("==");
	myExp2.unparse(p, 0);
	p.print(")");
    }
}

class NotEqualsNode extends EqualityBinExpNode {
    public NotEqualsNode(ExpNode exp1, ExpNode exp2) {
	super(exp1, exp2);
    }

    /** codeGen **/
    public void codeGen() {
	Codegen.genComment("NEQ");
	if (myExp1.returnSize() == 4 && myExp2.returnSize() == 4)
	    ((EqualityBinExpNode)this).codeGen("bne");
	else { // At least one is a double
	    /** Subtracting two non-equal values produces a non-zero result **/
	    // Evaluate
	    myExp1.codeGen();
	    if (myExp1.returnSize() == 4)
		intToDouble();
	    myExp2.codeGen();
	    if (myExp2.returnSize() == 4)
		intToDouble();
	    // Pop
	    Codegen.genPop(F0, 8);
	    Codegen.genPop(F2, 8);
	    // Subtract
	    Codegen.generate("sub.d", F0, F0, F2);
	    // Push onto stack
	    Codegen.genPush(F0, 8);

	    // Pop off MSB and LSB
	    Codegen.genPop(T0, 4);  // LSB
	    Codegen.genPop(T1, 4);  // MSB

	    String false_lab = Codegen.nextLabel();
	    String true_lab = Codegen.nextLabel();
	    String exit_lab = Codegen.nextLabel();
	    
	    // Force sign bit zero
	    Codegen.generate("and", T0, T0, "0x7FFFFFFF");

	    // Check values
	    Codegen.generate("bnez", T1, true_lab);
	    Codegen.generate("bnez", T0, true_lab);

	    // Must be equal
	    Codegen.generate("j", false_lab);
	    
	    // TRUE
	    Codegen.genLabel(true_lab);
	    Codegen.generate("li", T0, 1);
	    Codegen.generate("j", exit_lab);

	    // FALSE
	    Codegen.genLabel(false_lab);
	    Codegen.generate("li", T0, 0);

	    // EXIT
	    Codegen.genLabel(exit_lab);
	    Codegen.genPush(T0, 4);
	}
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print("(");
	myExp1.unparse(p, 0);
	p.print("!=");
	myExp2.unparse(p, 0);
	p.print(")");
    }
}

class LessNode extends EqualityBinExpNode {
    public LessNode(ExpNode exp1, ExpNode exp2) {
	super(exp1, exp2);
    }

    /** codeGen **/
    public void codeGen() {
	Codegen.genComment("LT");
	if (myExp1.returnSize() == 4 && myExp2.returnSize() == 4)
	    ((EqualityBinExpNode)this).codeGen("blt");
	else  // one is a double
	    EqualityBinExpNode.genLessWithDbl(myExp1, myExp2);
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print("(");
	myExp1.unparse(p, 0);
	p.print("<");
	myExp2.unparse(p, 0);
	p.print(")");
    }
}

class GreaterNode extends EqualityBinExpNode {
    public GreaterNode(ExpNode exp1, ExpNode exp2) {
	super(exp1, exp2);
    }

    /** codeGen **/
    public void codeGen() {
	Codegen.genComment("GT");
	if (myExp1.returnSize() == 4 && myExp2.returnSize() == 4)
	    ((EqualityBinExpNode)this).codeGen("bgt");
	else // DBL DBL
	    EqualityBinExpNode.genLessWithDbl(myExp2, myExp1);  // opposite of a less than
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print("(");
	myExp1.unparse(p, 0);
	p.print(">");
	myExp2.unparse(p, 0);
	p.print(")");
    }
}

class LessEqNode extends EqualityBinExpNode {
    public LessEqNode(ExpNode exp1, ExpNode exp2) {
	super(exp1, exp2);
    }

    /** codeGen **/
    public void codeGen() {
	Codegen.genComment("LEQ");
	if (myExp1.returnSize() == 4 && myExp2.returnSize() == 4)
	    ((EqualityBinExpNode)this).codeGen("ble");
	else // DBL DBL
	    EqualityBinExpNode.genLessEqWithDbl(myExp1, myExp2);
    }


    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print("(");
	myExp1.unparse(p, 0);
	p.print("<=");
	myExp2.unparse(p, 0);
	p.print(")");
    }
}

class GreaterEqNode extends EqualityBinExpNode {
    public GreaterEqNode(ExpNode exp1, ExpNode exp2) {
	super(exp1, exp2);
    }

    /** codeGen **/
    public void codeGen() {
	Codegen.genComment("GEQ");
	if (myExp1.returnSize() == 4 && myExp2.returnSize() == 4)
	    ((EqualityBinExpNode)this).codeGen("bge");
	else
	    EqualityBinExpNode.genLessEqWithDbl(myExp2, myExp1);	    
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print("(");
	myExp1.unparse(p, 0);
	p.print(">=");
	myExp2.unparse(p, 0);
	p.print(")");
    }
}


// **********************************************************************
// Subclasses of LogicalBinExpNode
// **********************************************************************

class AndNode extends LogicalBinExpNode {
    public AndNode(ExpNode exp1, ExpNode exp2) {
	super(exp1, exp2);
    }

    /** codeGen **/
    public void codeGen() {
	// Push first expression
	myExp1.codeGen();

	// Pop into register
	Codegen.genPop(T0, 4);

	// Generate label for short-circuit and true
	String exit_lab = Codegen.nextLabel();
	String false_lab = Codegen.nextLabel();

	// Exit if false
	Codegen.generate("beqz", T0, exit_lab);

	// If true, evaluate RHS
	myExp2.codeGen();
	Codegen.genPop(T0,4);

	// Generate exit label
	Codegen.genLabel(exit_lab, "AND-SHORT CIRCUIT");
	Codegen.genPush(T0, 4); //return is always in T0
	
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print("(");
	myExp1.unparse(p, 0);
	p.print("&&");
	myExp2.unparse(p, 0);
	p.print(")");
    }
}

class OrNode extends LogicalBinExpNode {
    public OrNode(ExpNode exp1, ExpNode exp2) {
	super(exp1, exp2);
    }

    /** codeGen **/
    public void codeGen() {
	// Push first expression
	myExp1.codeGen();

	// Pop into register
	Codegen.genPop(T0, 4);

	// Generate label for short-circuit and true
	String exit_lab = Codegen.nextLabel();
	String false_lab = Codegen.nextLabel();

	// Exit if true
	Codegen.generate("bnez", T0, exit_lab);

	// If true, evaluate RHS
	myExp2.codeGen();
	Codegen.genPop(T0,4);

	// Generate exit label
	Codegen.genLabel(exit_lab, "AND-SHORT CIRCUIT");
	Codegen.genPush(T0, 4); //return is always in T0
	
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
	p.print("(");
	myExp1.unparse(p, 0);
	p.print("||");
	myExp2.unparse(p, 0);
	p.print(")");
    }
}





