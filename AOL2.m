function[sol_final,media,desvio_padrao]=AOL2(Obj)

    tic;
       
    %% Behavioural parameters for this optimizer
    Dim = Obj.nVar;     % Variable number
    NP = 30;            % Population size
    % Separando a população entre: masc e fem
    % 70% serão leoas pride, 20% serão leoes pride e 10% leoes nomades
    Nleoas = 0.7*NP;
    Nleoes = 0.2*NP;
    Nleoes_nom = 0.1*NP;
    if Nleoes_nom <= 1
        Nleoes_nom = 2;
    end
%     Cr = 0.6;           % Crossover probability.
%     F = 0.3;            % Differential weight.
%     mi = 0;
%     sigma = 1;
    Bu = Obj.var_max*ones(1, Dim);
    Bl = Obj.var_min*ones(1, Dim);
    fun = Obj.fun;
    maxEvaluations = 500;
    maxIteracoes = 1;
    %%
    for jkk = 1:maxIteracoes
    % Initialize the main population
    Pop = repmat(Bl, NP, 1) + rand(NP, Dim) .* (repmat(Bu - Bl, NP, 1));
    % Avaliation the main population
    aux_Aptidao = fun(Pop);
    [Aptidao, ind1] = sort(aux_Aptidao);
    Pop1(:,:) = Pop(ind1(:),:);
    % Inicializando população leão pride
    leoes = Pop1(1:Nleoes,:);
    Aptidao_leao = Aptidao(1:Nleoes);
    % Inicializando população leoas pride
    leoas = Pop1(Nleoes+1:Nleoes+Nleoas,:);
    Aptidao_leoa = Aptidao(Nleoes+1:Nleoes+Nleoas,:);
    % Inicializando população leão nomade
    aux = Pop1(Nleoas+Nleoes+1:end,:);
    [Ap_leao_nom, ind] = min(Aptidao(Nleoas+Nleoes+1:end,:));
    leao_nom(1,:)= aux(ind,:);
    Aptidao_leao_nom(1) = Ap_leao_nom;
    %%    
    fref = Aptidao_leao(1);
    Lr = 0;
    Lrmax = 3;
    Sr = 0;
    Srmax = 3;
    %gcmax = round(0.3*Dim);
    gcmax = 10;
    Agemax = 3;

    % ITERAÇÕES

    
        for it = 1:maxEvaluations
%--------------------------------------------------------------------------
            % Fertilização
%--------------------------------------------------------------------------
            if fref <=  Aptidao_leao(1)
                Lr = Lr + 1;
            else
                Lr = 0;
                fref =  Aptidao_leao(1);
            end
            if Sr <= Srmax
                uc = 0;
                gc = 0;
                for ii=1:gcmax
                    leoa_mais = leoas(1,:);
                    r1 = rand();
                    r2 = rand();
                    k = randi(Dim);
                    delta = leoas(1,k) + (0.1*r2 - 0.05)*(leoes(1,k) - r1*leoas(1,k));
                    leoa_mais(1,k) = min(Bu(1,k),max(Bl(1,k),delta));
                    Aptidao_leoa_mais = fun(leoa_mais);
                    if Aptidao_leoa_mais < Aptidao_leoa(1)
                        uc = 1;
                        leoas(1,:) = leoa_mais;
                        Sr = 0;
                    end
                end
                if uc == 0
                        Sr = Sr + 1;
                end
            end
            
            Flag2Bu = leoes < Bl;
            Flag2Bl = leoes > Bu;
            leoes = (leoes.*(~(Flag2Bu+Flag2Bl)))+Bu.*Flag2Bu+Bl.*Flag2Bl;
            Flag4Bu = leoas < Bl;
            Flag4Bl = leoas > Bu;
            leoas =(leoas.*(~(Flag4Bu+Flag4Bl)))+Bu.*Flag4Bu+Bl.*Flag4Bl;
%--------------------------------------------------------------------------            
            % Acasalamento
%--------------------------------------------------------------------------
            % 1) Cruzamento
            j = 1;
            for i=1:Nleoes
                c1 = randi(Dim);
                c2 = randi(Dim);
                ind = randi(Nleoas);
                cruzamento(j,:) = [leoas(ind,1:c1) leoes(i,c1+1:end)];
                cruzamento(j+1,:) = [leoas(ind,c1+1:end) leoes(i,1:c1)];
                cruzamento(j+2,:) = [leoas(ind,1:c2) leoes(i,c2+1:end)];
                cruzamento(j+3,:) = [leoas(ind,c2+1:end) leoes(i,1:c2)];
                j = j + 4;
            end
                        
            % 2) Mutação
            mutacao = cruzamento;
            for k = 1:size(cruzamento,1)
                ind3 =  randi(Dim);
                mutacao(k,ind3) = unifrnd(Bl(ind3),Bu(ind3));
            end
            
            % Avaliação dos filhotes:
            Cubs = [cruzamento; mutacao];
            f_Cubs = fun(Cubs);
            [a,ii] = sort(f_Cubs);
            cubs(1,:) = Cubs(ii(1),:);
            f_cubs(1,:) = f_Cubs(ii(1));
            cubs(2,:) = Cubs(ii(2),:);
            f_cubs(2,:) = f_Cubs(ii(2));
            Age = 0;
            defesa = 0;
            
%--------------------------------------------------------------------------            
            % Função de crescimento dos filhotes
%--------------------------------------------------------------------------
            while (Age < Agemax && defesa == 0)
                newMutation = normrnd(cubs,0.1);
                f_mutacao = fun(newMutation);
                for i = 1:2
                   if f_mutacao(i) < f_cubs(i)
                       f_cubs(i) = f_mutacao(i);
                       cubs(i) = newMutation(i);
                   end
                end

%--------------------------------------------------------------------------               
        % Defesa de territorio
%--------------------------------------------------------------------------
                if (Aptidao_leao_nom(1) <= Aptidao_leao(1) && Aptidao_leao_nom(1) < f_cubs(1) && Aptidao_leao_nom(1) < f_cubs(2))
                    Aptidao_leao(1) = Aptidao_leao_nom(1);
                    leoes(1,:) = leao_nom(1,:);
                    cubs(:,:) = inf;
                    defesa = 1;
                    Age = 0;
                else
                    if Lr < Lrmax
                         for i = 2: Nleoes_nom
                             leao_nom(i,:) = unifrnd(Bl,Bu);
                         end
                    else
                        for i = 2:Nleoes_nom
                            leao_nom(i,:) = normrnd(leoes(1,:),0.1);
                        end
                    end
                    aux_Apti = fun(leao_nom);
                    aux1 = leao_nom;
                    [Aptidao_leao_nom, ind1] = sort(aux_Apti);
                    leao_nom = aux1(ind1(:),:);
                    defesa = 0;
                    Age = Age + 1;
                end
            end
            
        
        %% Aquisição territorial
            if Age >= Agemax
                if  Aptidao_leao(1,:) > f_cubs(1,:)
                        leoes(1,:) = cubs(1,:);
                        Aptidao_leao(1,:) = f_cubs(1,:);
                else
                    if  Aptidao_leao(Nleoes,:) > f_cubs(1,:)
                        leoes(Nleoes,:) = cubs(1,:);
                        Aptidao_leao(Nleoes,:) = f_cubs(1,:);
                    end
                end
                Xold = leoas(1,:);
                if  Aptidao_leoa(1,:) > f_cubs(2,:)
                        leoas(1,:) = cubs(2,:);
                        Aptidao_leoa(1,:) = f_cubs(2,:);
                        Sr = 0;
                else
                    if  Aptidao_leoa(Nleoas,:) > f_cubs(2,:)
                        leoas(Nleoas,:) = cubs(2,:);
                        Aptidao_leoa(Nleoas,:) = f_cubs(2,:);
                    end
                end
            end
            % Update Best Cost
            Pop2 = [leoes; leoas];
            Aptidao2 = [Aptidao_leao; Aptidao_leoa];
            [Aptidao3, ind2] = sort(Aptidao2);
            Pop3 = Pop2(ind2(:),:);
            leoes = Pop3(1:Nleoes,:);
            Aptidao_leao = Aptidao3(1:Nleoes);
            leoas = Pop3(Nleoes+1:Nleoes+Nleoas,:);
            Aptidao_leoa = Aptidao3(Nleoes+1:Nleoes+Nleoas,:);
            
            BestCost(it) = Aptidao_leao(1);
            bestPar (it,:) = leoes(1,:);
            disp(['Iteração ' num2str(it) ': Solução ótima = ' num2str(BestCost(it))]);
            
        end %fim for it
        
        bestParjj (jkk,:) = min(bestPar);
        BestCostjj(jkk)= min(BestCost);
        % Show Iteration Information
        disp(['Iteração ' num2str(jkk) ': Solução ótima = ' num2str(BestCostjj(jkk))]);
    end % fim for jj
    
    ttempo = toc;
    
    sol_final = bestParjj;
    media = mean(BestCostjj)
    minCost = min(BestCostjj)
    desvio_padrao = std(BestCostjj)
end
    
   
    
