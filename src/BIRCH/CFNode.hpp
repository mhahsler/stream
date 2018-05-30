//
// Created by Dede on 08.09.2016.
//

#ifndef CFTREE_CFNODE_H
#define CFTREE_CFNODE_H

namespace CF {
    class CFNode {
    public:
        virtual ~CFNode(){};
        virtual int getLength() = 0;
    };
}


#endif //CFTREE_CFNODE_H
