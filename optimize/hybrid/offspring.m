function [m_n] = offspring(m_o,k,m_min,m_max,tmp)
    
    % this function generates new model from given old model
    % by the random shift in one dimension only

%     nop = size(m_o,1);          % number of parameters
    m_n = m_o;         % produced new model
    
    for ntry = 1:100
        dif = rand-0.5;
        if dif < 0
            sign = -1;
        end
        if dif >= 0
            sign = 1;
        end
        m_t = m_o(k)+sign*rand*tmp*(m_max(k)-m_min(k));
        if m_t>=m_min(k) && m_t<=m_max(k)
            break;
        end
    end
    
    if ntry >= 100
        error('could not find search point from cauchy distribution')
    end
    
    m_n(k) = m_t;

end
    

