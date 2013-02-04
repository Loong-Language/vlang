
#include "Parser.h"
#include "Type.h"
#include "Ast.h"
using namespace vlang;

TypeInfo::~TypeInfo(){
}

DimensionInfo::~DimensionInfo(){
	if( dimension.first != nullptr ){
		delete dimension.first;
	}

	if( dimension.second != nullptr ){
		delete dimension.second;
	}
}

bool vlang::operator==( DimensionInfo &lhs, DimensionInfo &rhs){
	return (lhs.getKind() == rhs.getKind() &&
	     	lhs.getLhs()  == rhs.getLhs()  &&
		    lhs.getRhs()  == rhs.getRhs());
}

bool vlang::operator!=( DimensionInfo &lhs, DimensionInfo &rhs){
	return !(lhs == rhs);
}

bool vlang::operator==(TypeInfo &lhs, TypeInfo &rhs)
{
	return (lhs.getDimension() == rhs.getDimension() &&
		    lhs.getDataType()  == rhs.getDataType()  &&
			lhs.getNetType()   == rhs.getNetType()   &&
			lhs.getSigning()   == rhs.getSigning()    );
}

bool vlang::operator!=(TypeInfo &lhs, TypeInfo &rhs){
	return !(lhs == rhs);
}

void DeclTypeInfo::addArrayDimension(DimensionInfo dim){

}