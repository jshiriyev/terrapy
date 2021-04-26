function [m_n,E_n] = selection(m_o,E_o)
    
%     nop = size(m_o,1);
%     nom = size(m_o,2);
%     
%     m_n = zeros(nop,nom);
%     E_n = zeros(1,nom);
    
    m_n = m_o;
    E_n = E_o;
    
    [~,lcmp] = min(E_o);
    [~,lcxp] = max(E_o);
    
    m_n(:,lcxp) = m_n(:,lcmp);
    E_n(1,lcxp) = E_o(1,lcmp);
    
%     for i = 1:nom
%         m_n(:,i) = m_o(:,lcmp);
%         E_n(1,i) = E_o(1,lcmp);
%     end
    
end
    

