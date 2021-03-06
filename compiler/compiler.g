#header
<<
#include <string>
#include <iostream>
#include <map>
using namespace std;

// struct to store information about tokens
typedef struct {
  string kind;
  string text;
} Attrib;

// function to fill token information (predeclaration)
void zzcr_attr(Attrib *attr, int type, char *text);

// fields for AST nodes
#define AST_FIELDS string kind; string text;
#include "ast.h"

// macro to create a new AST node (and function predeclaration)
#define zzcr_ast(as,attr,ttype,textt) as=createASTnode(attr,ttype,textt)
AST* createASTnode(Attrib* attr, int ttype, char *textt);
>>

<<
#include <cstdlib>
#include <cmath>

//global structures
map<string,bool> block;
AST *root;


// function to fill token information
void zzcr_attr(Attrib *attr, int type, char *text) {
//  if (type == NUM) {
//    attr->kind = "intconst";
//    attr->text = text;
//  }
//  else {
    attr->kind = text;
    attr->text = "";
//  }
}

// function to create a new AST node
AST* createASTnode(Attrib* attr, int type, char* text) {
  AST* as = new AST;
  as->kind = attr->kind; 
  as->text = attr->text;
  as->right = NULL; 
  as->down = NULL;
  return as;
}


/// create a new "list" AST node with one element
AST* createASTlist(AST *child) {
 AST *as=new AST;
 as->kind="list";
 as->right=NULL;
 as->down=child;
 return as;
}

AST *findASTListDef(string id) {
  AST *n = root->down;
  while (n != NULL and n->down->kind != id) n = n->right;
  return n;
}

/// get nth child of a tree. Count starts at 0.
/// if no such child, returns NULL
AST* child(AST *a,int n) {
AST *c=a->down;
for (int i=0; c!=NULL && i<n; i++) c=c->right;
return c;
}



/// print AST, recursively, with indentation
void ASTPrintIndent(AST *a,string s)
{
  if (a==NULL) return;

  cout<<a->kind;
  if (a->text!="") cout<<"("<<a->text<<")";
  cout<<endl;

  AST *i = a->down;
  while (i!=NULL && i->right!=NULL) {
    cout<<s+"  \\__";
    ASTPrintIndent(i,s+"  |"+string(i->kind.size()+i->text.size(),' '));
    i=i->right;
  }
  
  if (i!=NULL) {
      cout<<s+"  \\__";
      ASTPrintIndent(i,s+"   "+string(i->kind.size()+i->text.size(),' '));
      i=i->right;
  }
}

/// print AST 
void ASTPrint(AST *a)
{
  while (a!=NULL) {
    cout<<" ";
    ASTPrintIndent(a,"");
    a=a->right;
  }
}

/* Aggregate sum for each level of tree
   and evaluate the following levels */
int sum_list(AST *a) {
  int sum = 0;
  if (a->kind == "[") {
    AST* son = child(a,0);
    while (son != NULL) {
      if (son->kind == "[") 
        sum += sum_list(son);
      else
        sum += atoi(son->kind.c_str());
      son = son->right;
    }
  } 
  else if (a->kind == "#") {
    AST* l1 = findASTListDef(child(a,0)->kind);
    AST* l2 = findASTListDef(child(a,1)->kind);
    sum += sum_list(child(l1,1)) + sum_list(child(l2,1));
  }

  return sum;
}

/* Aggregate decrement for each level of tree
   and evaluate the following levels */
int decrement_list(AST *a) {
  int rest = 0;
  if (a->kind == "[") {
    AST* son = child(a,0);
    while (son != NULL) {
      if (son->kind == "[") 
        rest -= decrement_list(son);
      else
        rest -= atoi(son->kind.c_str());
      son = son->right;
    }
  } 
  else if (a->kind == "#") {
    AST* l1 = findASTListDef(child(a,0)->kind);
    AST* l2 = findASTListDef(child(a,1)->kind);
    rest -= decrement_list(child(l1,1)) - decrement_list(child(l2,1));
  }

  return rest;
}

/* Aggregate product for each level of tree
   and evaluate the following levels */
int prod_list(AST *a) {
  int prod = 1;
  if (a->kind == "[") {
    AST* son = child(a,0);
    while (son != NULL) {
      if (son->kind == "[") 
        prod *= prod_list(son);
      else
        prod *= atoi(son->kind.c_str());
      son = son->right;
    }
  } 
  else if (a->kind == "#") {
    AST* l1 = findASTListDef(child(a,0)->kind);
    AST* l2 = findASTListDef(child(a,1)->kind);
    prod *= prod_list(child(l1,1)) * prod_list(child(l2,1));
  }

  return prod;
}

int reduce(AST *a) {
  AST* node_id;
  if (a == NULL) return 0;
  else if (child(a,0)->kind == "+") {
    node_id = findASTListDef(child(a,1)->kind);
    return sum_list(child(node_id,1));
  }
  else if (child(a,0)->kind == "-"){
    node_id = findASTListDef(child(a,1)->kind);
    return decrement_list(child(node_id,1));
  }
  else if (child(a,0)->kind == "*"){
    node_id = findASTListDef(child(a,1)->kind);
    return prod_list(child(node_id,1));
  }

}

void run(AST *a) {
  if (a == NULL) return;
  else if (a->kind == "list")
    return run(child(a,0));
  else if (a->kind == "=") {
    if (child(a,1)->kind == "lreduce") {
      int val = reduce(child(a,1));
      cout << child(a,0)->kind << " = " << val << endl;
    }   
  }
  run(a->right);
}

int main() {
  root = NULL;
  ANTLR(lists(&root), stdin);
  ASTPrint(root);
  run(root);
}
>>

#lexclass START
#token LMAP "lmap"
#token LREDUCE "lreduce"
#token LFILTER "lfilter"
#token CONCAT "\#"
#token COMA "\,"
#token NUM "[0-9]+"
#token PLUS "\+"
#token MINUS "\-"
#token LBRACKET "\["
#token RBRACKET "\]"
#token MULT "\*"
#token DIV "\/"
#token SPACE "[\ \n]" << zzskip();>>
#token PRINT "print"
#token NOT "\!"
#token AND "\&&"
#token OR "\|\|"
#token BOOLOPERATOR "\<|\>|\<=|\>="
#token EQUIVALENCE "\=\=|\!\="
#token ID "[a-zA-z]+[0-9]*"
#token ASIG "\="

lists:          (list_oper)* "@"! <<#0=createASTlist(_sibling);>> ;
list_oper:      ID ASIG^ (list|func|concat) | PRINT^ ID;
list:           (LBRACKET^ (recurse (list_expr)*|) RBRACKET!);
list_expr:      (COMA! (NUM|list));
recurse:        (NUM|list);
func:           (LMAP^ exprbin | LREDUCE^ op ID | LFILTER^ exprbool ID);
exprbin:        op NUM ID;
op:             (PLUS^|MINUS^|MULT^|DIV^);
exprbool:       (BOOLOPERATOR^|EQUIVALENCE^) NUM;
concat:         ID CONCAT^ ID;
