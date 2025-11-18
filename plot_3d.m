cd('/Users/ellag/Library/CloudStorage/GoogleDrive-elhe2720@colorado.edu/My Drive/Colorado/PhD/PROJECTS/')



%Load in data
coord_filename = "ball_drop_data_with_3D_trial1.xlsx" %CHANGE
imgcoordsRC_PC = readtable(coord_filename);

%Data wrangle
imgcoordsRC_head = zeros(length(imgcoordsRC)/2, 3)
imgcoordsRC_base =  zeros(length(imgcoordsRC)/2, 3)

imgcoordsRC(:,4) = ceil((1:size(imgcoordsRC,1))/2)
imgcoordsRC(:,4) = [1:80]

h = 1
b = 1

for lc = 1:length(imgcoordsRC)
    if mod(imgcoordsRC(lc,4), 2) == 0
        imgcoordsRC_head(h,1:3) = imgcoordsRC(lc,1:3)
        h = h +1;
  % x is even
    else
  % x is odd
        imgcoordsRC_base(h,1:3) = imgcoordsRC(lc,1:3)
        b = b +1;
    end
end

imgcoordsRC_head(:,4) = [1:40]
imgcoordsRC_base(:,4) = [1:40]


%Plot without numbers
figure
hold on
for k = 1:size(imgcoordsRC,1)
    plot3([imgcoordsRC(k,59);imgcoordsRC(k,56)], [imgcoordsRC(k,60);imgcoordsRC(k,57)], [imgcoordsRC(k,61);imgcoordsRC(k,59)], '-k')
    scatter3(imgcoordsRC(k,59), imgcoordsRC(k,60), imgcoordsRC(k,61), 10, 'r', 'filled')
    scatter3(imgcoordsRC(k,56), imgcoordsRC(k,57), imgcoordsRC(k,58),10, 'g', 'filled')
end
hold off
grid on


%Plot with numbers
figure;
hold on

for k = 1:size(imgcoordsRC_head,1)
    % Draw line connecting head and base
    plot3([imgcoordsRC_head(k,1); imgcoordsRC_base(k,1)], ...
          [imgcoordsRC_head(k,2); imgcoordsRC_base(k,2)], ...
          [imgcoordsRC_head(k,3); imgcoordsRC_base(k,3)], '-k')
    
    % Plot head and base points
    scatter3(imgcoordsRC_base(k,1), imgcoordsRC_base(k,2), imgcoordsRC_base(k,3), 5, 'r', 'filled')
    scatter3(imgcoordsRC_head(k,1), imgcoordsRC_head(k,2), imgcoordsRC_head(k,3), 5, 'g', 'filled')
    
    % Add label numbers from the 4th column
    text(imgcoordsRC_head(k,1), imgcoordsRC_head(k,2), imgcoordsRC_head(k,3), ...
        num2str(imgcoordsRC_head(k,4)), 'VerticalAlignment','bottom', 'HorizontalAlignment','right', 'Color','k', 'FontSize',10)
end

hold off
grid on
xlabel('X'); ylabel('Y'); zlabel('Z');
view(3)  % 3D view




%Plot with sand (plane of best fit)


% Extract base coordinates
Xb = imgcoordsRC_base(:,1);
Yb = imgcoordsRC_base(:,2);
Zb = imgcoordsRC_base(:,3);

% Fit plane to base points using least squares: Z = a*X + b*Y + c
A = [Xb, Yb, ones(size(Xb))];
coeff = A\Zb;
a = coeff(1); b_ = coeff(2); c = coeff(3);

% Create grid for plane
[Xgrid, Ygrid] = meshgrid(linspace(min(Xb)-1, max(Xb)+1, 20), ...
                          linspace(min(Yb)-1, max(Yb)+1, 20));
Zgrid = a*Xgrid + b_*Ygrid + c;

% Start figure
figure; hold on;

% Plot plane
mesh(Xgrid, Ygrid, Zgrid, 'FaceAlpha',0.3, 'EdgeColor','b');


% Plot lines and points
for k = 1:size(imgcoordsRC,1)
    % Line from head to base
    plot3([imgcoordsRC(k,59), imgcoordsRC(k,56)], ...
          [imgcoordsRC(k,60), imgcoordsRC(k,57)], ...
          [imgcoordsRC(k,61), imgcoordsRC(k,58)], '-k')
    
    % Head and base points
    scatter3(imgcoordsRC(k,59), imgcoordsRC(k,60), imgcoordsRC(k,61), 5, [0, 0.6, 0], 'filled') % green head
    scatter3(imgcoordsRC(k,56), imgcoordsRC(k,57), imgcoordsRC(k,58), 5, [0.65, 0.16, 0.16], 'filled') % brown base
    
    % Head labels
    text(imgcoordsRC(k,59), imgcoordsRC(k,60), imgcoordsRC(k,61), ...
        num2str(imgcoordsRC(k,4)), 'VerticalAlignment','bottom', ...
        'HorizontalAlignment','right', 'Color','k', 'FontSize',10)
end

hold off
grid on
xlabel('X'); ylabel('Y'); zlabel('Z');
axis equal
rotate3d on
zoom on
pan on
view(3)
title('Eel Head & Base Points with Fitted Sand Plane')




figure
hold on
grid on

% Extract base coordinates
base_x_all = imgcoordsRC{:,56};
base_y_all = imgcoordsRC{:,57};
base_z_all = imgcoordsRC{:,58};

valid_idx = ~isnan(base_x_all) & ~isnan(base_y_all) & ~isnan(base_z_all);
Xb = base_x_all(valid_idx);
Yb = base_y_all(valid_idx);
Zb = base_z_all(valid_idx);

% Fit plane to base points using least squares: Z = a*X + b*Y + c
A = [Xb, Yb, ones(size(Xb))];
coeff = A\Zb;
a = coeff(1); b_ = coeff(2); c = coeff(3);

% Create grid for plane
[Xgrid, Ygrid] = meshgrid(linspace(min(Xb)-1, max(Xb)+1, 20), ...
                          linspace(min(Yb)-1, max(Yb)+1, 20));
Zgrid = a*Xgrid + b_*Ygrid + c;

% Start figure
figure; hold on;

% Plot plane
mesh(Xgrid, Ygrid, Zgrid, 'FaceAlpha',0.3, 'EdgeColor','b');

figure
hold on
grid on

for k = 1:size(imgcoordsRC,1)
    % Extract coordinates for base and head
    base_x = imgcoordsRC{k,56};
    base_y = imgcoordsRC{k,57};
    base_z = imgcoordsRC{k,58};
    head_x = imgcoordsRC{k,59};
    head_y = imgcoordsRC{k,60};
    head_z = imgcoordsRC{k,61};

    % Skip if any coordinate is NaN
    if any(isnan([base_x, base_y, base_z, head_x, head_y, head_z]))
        continue
    end

    % Line from base to head
    %plot3([base_x, head_x], [base_y, head_y], [base_z, head_z], '-k')
    
    % Plot head point (green)
    %scatter3(head_x, head_y, head_z, 30, [0, 0.6, 0], 'filled')
    
    % Plot base point (brown)
    scatter3(base_x, base_y, base_z, 30, [0.65, 0.16, 0.16], 'filled')
    
    % Add label at head point from column 4
    text(head_x, head_y, head_z, num2str(imgcoordsRC{k,4}), ...
         'VerticalAlignment', 'bottom', 'HorizontalAlignment', 'right', ...
         'Color', 'k', 'FontSize', 10)
end

hold off

%Load in data
coord_filename = "ball_drop_data_with_3D_apple_trial1.xlsx" %CHANGE
imgcoordsRC = readtable(coord_filename);

imgcoordsRC = imgcoordsRC(imgcoordsRC{:,3} == 1, :);
%VISUALISING WITH BALL DROP
figure
hold on
grid on

for k = 1:size(imgcoordsRC,1)
    % Extract coordinates for base, head, and ball points
    base_x = imgcoordsRC{k,56};
    base_y = imgcoordsRC{k,57};
    base_z = imgcoordsRC{k,58};
    
    head_x = imgcoordsRC{k,59};
    head_y = imgcoordsRC{k,60};
    head_z = imgcoordsRC{k,61};

    ball_first_x = imgcoordsRC{k,50};
    ball_first_y = imgcoordsRC{k,51};
    ball_first_z = imgcoordsRC{k,52};
    
    ball_drop_x = imgcoordsRC{k,53};  % ball_hit_X
    ball_drop_y = imgcoordsRC{k,54};  % ball_hit_Y
    ball_drop_z = imgcoordsRC{k,55};  % ball_hit_Z

    % Skip if any key coordinates are missing
    if any(isnan([base_x, base_y, base_z, head_x, head_y, head_z, ...
                  ball_first_x, ball_first_y, ball_first_z, ...
                  ball_drop_x, ball_drop_y, ball_drop_z]))
        continue
    end

        % Line from base to head
    plot3([base_x, head_x], [base_y, head_y], [base_z, head_z], '-k')
    
    % Plot head point (green)
    scatter3(head_x, head_y, head_z, 15, [0, 0.6, 0], 'filled')
    
    % --- Plot the eel base ---
    scatter3(base_x, base_y, base_z, 15, [0.65, 0.16, 0.16], 'filled') % brown base

    % --- Plot the ball drop as a pink star ---
    scatter3(ball_drop_x, ball_drop_y, ball_drop_z, 100, 'p', 'MarkerEdgeColor', 'k', ...
             'MarkerFaceColor', [1, 0.4, 0.7]) % pink star
    
    % --- Line from ball drop to first view ---
    plot3([ball_drop_x, ball_first_x], [ball_drop_y, ball_first_y], [ball_drop_z, ball_first_z], ...
          '-m', 'LineWidth', 1.5) % magenta line

    % --- Add label (optional) ---
    text(head_x, head_y, head_z, num2str(imgcoordsRC{k,4}), ...
         'VerticalAlignment', 'bottom', 'HorizontalAlignment', 'right', ...
         'Color', 'k', 'FontSize', 10)
end

xlabel('X'); ylabel('Y'); zlabel('Z');
title('PC Ball Drop and Eel 3D Visualization');
axis equal
xlim([-1.6 1.6])
ylim([-1.2 0.8])
hold off

%writetable(imgcoordsRC, "ella_ball_drop_data_with_3D.csv")
