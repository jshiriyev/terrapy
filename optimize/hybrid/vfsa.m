function R2 = vfsa(m_min,m_max,nog,nom,nop)

    % R2 is saving any model and its energy which evaluated

    R2 = zeros(nog*nom,1+nop);

    m_1 = zeros(nop,nom);
    m_2 = zeros(nop,nom);
    E_1 = zeros(1,nom);
    E_2 = zeros(1,nom);
    
    for j = 1:nom
        m_1(:,j) = m_min+rand(nop,1).*(m_max-m_min);
        E_1(j) = E_func(m_1(:,j));
        R2(j,:) = [E_1(j),m_1(:,j)'];
    end
    
    [m_1,E_1] = selection(m_1,E_1);
    
    for i = 2:nog
        T = temperature(i,nog);
        for j = 1:nom
            for k = 1:nop
                m_2(:,j) = offspring(m_1(:,j),k,m_min,m_max,T);
                if k < nop
                    E_2(j) = neighbor(m_2(:,j),R2(1:(i-1)*nom,:),m_min,m_max);
                else
                    E_2(j) = E_func(m_2(:,j));
                    R2((i-1)*nom+j,:) = [E_2(j),m_2(:,j)'];
                end
                d_E = E_2(j)-E_1(j);
                if d_E <= 0
                    m_1(k,j) = m_2(k,j);
                    E_1(j) = E_2(j);
                else
                    if T > rand
                        m_1(k,j) = m_2(k,j);
                        E_1(j) = E_2(j);
                    end
                end
            end
        end
        [m_1,E_1] = selection(m_1,E_1);
    end
    
end
