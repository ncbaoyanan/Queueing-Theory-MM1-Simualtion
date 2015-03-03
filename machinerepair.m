function [Pi,w] =  machinerepair(r,u,k)
    Pi = ones(k+1,1);
    for i = 1:k
        Pi(1+i) = Pi(i)*(u/r)*((k+1-i)/(i));
    end
    Pi = Pi/sum(Pi);
    w = sum(Pi.*(0:1:k)');
end