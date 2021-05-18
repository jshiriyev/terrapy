classdef meshInterchange
    %UNTITLED Summary of this class goes here
    %   Detailed explanation goes here
    
    properties
        elementList = [];
        gridHalfLength;           
        nodesList = struct('xLocation',{},'yLocation',{});
        fracMap = struct('node_1',{},'node_2',{});
        
        productionFracMap;
        productionNodeList;
    end
    
    methods
        function obj = meshInterchange(elementData,gridSize)
            %UNTITLED Construct an instance of this class
            %   Detailed explanation goes here
            obj.elementList = elementData;
            obj.gridHalfLength = gridSize;
            obj = obj.createProductionMesh();
            [obj.productionNodeList,obj.productionFracMap] = obj.convertDDMMeshToProductionMesh();
        end
        
        function sizeOfMesh = returnSizeOfDDMMesh(obj)
            [sizeOfMesh,sizeA] = size(obj.elementList);
        end
        
        function sizeOfProductionMesh = returnSizeOfProductionMesh(obj)
            [sizeOfProductionMesh,sizeA] = size(obj.nodesList);
        end
        
        function [location,isAlreadyPresent] = isNodeAvailable(obj,nodeLocationX,nodeLocationY)
            N = obj.returnSizeOfProductionMesh();
            isAlreadyPresent = 0;
            location = 0;
            for i = 1:N
                if(obj.nodesList(i).xLocation == nodeLocationX && obj.nodesList(i).yLocation == nodeLocationY)
                    isAlreadyPresent = 1;
                    location = i;
                end
            end
        end
        
        function obj = addNode(obj,nodeX,nodeY)
            [location,isAlreadyPresent] = isNodeAvailable(obj,nodeX,nodeY);
            if(isAlreadyPresent == 0)
                nodeToAdd.xLocation = nodeX;
                nodeToAdd.yLocation = nodeY;   
                obj.nodesList =[obj.nodesList;nodeToAdd];
            end            
        end
        
        function obj = addFracMap(obj,tempFracMap)                
                obj.fracMap = [obj.fracMap;tempFracMap];                         
        end
        
        function obj = storeFracMap(obj)
             N = obj.returnSizeOfDDMMesh();
             for i = 1:N
                [node_1_x,node_1_y,node_2_x,node_2_y] = obj.returnNodesOfElement(i);
                [nodeNo1,isAlreadyPresent] = obj.isNodeAvailable(node_1_x,node_1_y);
                [nodeNo2,isAlreadyPresent] = obj.isNodeAvailable(node_2_x,node_2_y);                
                
                tempFracMap.node_1 = nodeNo1;
                tempFracMap.node_2 = nodeNo2;                                
                
                obj = obj.addFracMap(tempFracMap);
             end
        end
        function obj = storeAllNodes(obj)
            
             N = obj.returnSizeOfDDMMesh();
             
             for i = 1:N
                    [node_1_x,node_1_y,node_2_x,node_2_y] = obj.returnNodesOfElement(i);
                    obj = obj.addNode(node_1_x,node_1_y);
                    obj = obj.addNode(node_2_x,node_2_y);                                        
             end
        end
        
        function [node_1_x,node_1_y,node_2_x,node_2_y] = returnNodesOfElement(obj,elementNo)
                    halfLengthOfFractureElement = obj.gridHalfLength;
                    angleOfElement = obj.elementList(elementNo).thetaNormal;
                    xCenter =  obj.elementList(elementNo).xLocation;
                    yCenter =  obj.elementList(elementNo).yLocation;
                     node_1_x = xCenter+halfLengthOfFractureElement*cos(angleOfElement);
                    node_2_x = xCenter-halfLengthOfFractureElement*cos(angleOfElement);
                    % y nodes
                    node_1_y = yCenter+halfLengthOfFractureElement*sin(angleOfElement);
                    node_2_y = yCenter-halfLengthOfFractureElement*sin(angleOfElement);
        end
         
        function obj = createProductionMesh(obj)
            obj = obj.storeAllNodes;
            obj = obj.storeFracMap;            
        end
        
        function [generatedFracMap_1,generatedFracMap_2] = ...
                    generateFracMapInProductionFormat(obj)
                N = obj.returnSizeOfDDMMesh();
                generatedFracMap_1 = zeros(N,1);
                generatedFracMap_2 = zeros(N,1);
                for i = 1:N
                    generatedFracMap_1(i) = obj.fracMap(i).node_1;
                    generatedFracMap_2(i) = obj.fracMap(i).node_2;                
                end
        end
        
        function [generatedNodeMap_X,generatedNodeMap_Y] = ...
                    generateNodeMapInProductionFormat(obj)
                N = obj.returnSizeOfProductionMesh();
                generatedNodeMap_X = zeros(N,1);
                generatedNodeMap_Y = zeros(N,1);
                for i = 1:N
                    generatedNodeMap_X(i) = obj.nodesList(i).xLocation;
                    generatedNodeMap_Y(i) = obj.nodesList(i).yLocation;                
                end
        end
        
        function nodes = returnNodeMap(obj)
            [generatedNodeMap_X,generatedNodeMap_Y] = ...
                    obj.generateNodeMapInProductionFormat();
                nodes = [generatedNodeMap_X generatedNodeMap_Y];
        end
        
        function fracMap = returnFracMap(obj)
            [generatedFracMap_1,generatedFracMap_2] = ...
                    obj.generateFracMapInProductionFormat();
                fracMap = [generatedFracMap_1 generatedFracMap_2];
        end
        
        function [nodes,fracMap] = convertDDMMeshToProductionMesh(obj)
            nodes = obj.returnNodeMap();
            fracMap = obj.returnFracMap();
        end        
    end
end

